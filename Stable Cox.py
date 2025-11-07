import numpy as np
import pandas as pd
import torch
import os
import argparse
import time
import warnings
warnings.filterwarnings('ignore')

from algorithm.DWR import DWR
from algorithm.SRDO import SRDO
from utils import setup_seed, get_cov_mask

from lifelines import CoxPHFitter
from sklearn.preprocessing import StandardScaler
import multiprocessing as mp
from concurrent.futures import ProcessPoolExecutor
import psutil


def get_args():
    parser = argparse.ArgumentParser(description="StableCox feature selection for real data", 
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("--data_csv", type=str, required=True, help="Path to your CSV")
    parser.add_argument("--duration_col", type=str, default="Survival_months")
    parser.add_argument("--event_col", type=str, default="Survival_status")
    parser.add_argument("--reweighting", choices=["None", "DWR", "SRDO"], default="DWR")
    parser.add_argument("--decorrelation_type", choices=["global", "group"], default="global")
    parser.add_argument("--order", type=int, default=1)
    parser.add_argument("--iters_balance", type=int, default=1000)
    parser.add_argument("--seed", type=int, default=3)
    parser.add_argument("--p_threshold", type=float, default=0.05, help="Significance level")
    parser.add_argument("--sample_size", type=int, default=100000, help="Number of samples to use (0 for all)")
    parser.add_argument("--penalizer", type=float, default=0.1, help="Regularization strength")
    parser.add_argument("--output_dir", type=str, default="./results", help="Output directory for results")
    
    # Parallel computing parameters
    parser.add_argument("--num_workers", type=int, default=mp.cpu_count(), help="Number of CPU workers")
    parser.add_argument("--gpu_ids", type=str, default="all", help="GPU IDs to use (e.g., '0,1,2,3' or 'all')")
    parser.add_argument("--batch_size", type=int, default=50000, help="Batch size for large data processing")
    return parser.parse_args()


def setup_parallel_environment(args):
    """Set up the parallel computing environment"""
    # Configure CPU parallelization
    torch.set_num_threads(args.num_workers)
    os.environ["OMP_NUM_THREADS"] = str(args.num_workers)
    os.environ["MKL_NUM_THREADS"] = str(args.num_workers)
    
    # Configure GPU devices
    if torch.cuda.is_available():
        if args.gpu_ids == "all":
            device_ids = list(range(torch.cuda.device_count()))
        else:
            device_ids = [int(x) for x in args.gpu_ids.split(",")]
        
        print(f"Using GPUs: {device_ids}")
        torch.cuda.set_device(device_ids[0])
        device = torch.device(f"cuda:{device_ids[0]}")
    else:
        device_ids = []
        device = torch.device("cpu")
    
    return device, device_ids


def process_data_batch(batch_data, batch_idx, total_batches):
    """Process each data batch (used for parallelization)"""
    print(f"Processing batch {batch_idx + 1}/{total_batches}, shape: {batch_data.shape}")
    # Custom data processing logic can be added here
    # e.g., normalization, feature engineering, etc.
    return batch_data


def parallel_data_processing(X_df, args):
    """Parallelized data preprocessing"""
    n_samples = len(X_df)
    batch_size = args.batch_size
    n_batches = (n_samples + batch_size - 1) // batch_size
    
    print(f"Starting parallel data processing: {n_samples} samples, {n_batches} batches, {batch_size} per batch")
    
    # Split into batches
    batches = []
    for i in range(n_batches):
        start_idx = i * batch_size
        end_idx = min((i + 1) * batch_size, n_samples)
        batch = X_df.iloc[start_idx:end_idx]
        batches.append((batch, i, n_batches))
    
    # Use multiprocessing for parallel processing
    with ProcessPoolExecutor(max_workers=args.num_workers) as executor:
        results = list(executor.map(
            lambda x: process_data_batch(*x), batches
        ))
    
    # Combine results
    processed_data = pd.concat(results, axis=0)
    return processed_data


def optimized_DWR(X, args, device, device_ids):
    """Optimized DWR implementation supporting multi-GPU"""
    cov_mask = get_cov_mask(np.zeros(X.shape[1])) if args.decorrelation_type == "global" else get_cov_mask(np.ones(X.shape[1]))
    
    # Use multiple GPUs if available
    if len(device_ids) > 1:
        print(f"Using {len(device_ids)} GPUs for DWR computation")
        batch_size = len(X) // len(device_ids)
        weights = []
        
        for i, gpu_id in enumerate(device_ids):
            start_idx = i * batch_size
            end_idx = (i + 1) * batch_size if i < len(device_ids) - 1 else len(X)
            X_batch = X[start_idx:end_idx]
            
            with torch.cuda.device(gpu_id):
                W_batch = DWR(
                    X_batch, 
                    cov_mask=cov_mask, 
                    order=args.order, 
                    num_steps=args.iters_balance,
                    device=torch.device(f"cuda:{gpu_id}"),
                    logger=None
                )
                weights.append(W_batch)
        
        # Merge weights
        W = np.vstack(weights)
    else:
        # Single-GPU version
        W = DWR(
            X, 
            cov_mask=cov_mask, 
            order=args.order, 
            num_steps=args.iters_balance,
            device=device,
            logger=None
        )
    
    return W


def check_memory_usage():
    """Check current memory usage"""
    memory_info = psutil.virtual_memory()
    print(f"Memory usage: {memory_info.used / 1024**3:.1f}GB / {memory_info.total / 1024**3:.1f}GB ({memory_info.percent}%)")
    
    if torch.cuda.is_available():
        for i in range(torch.cuda.device_count()):
            allocated = torch.cuda.memory_allocated(i) / 1024**3
            cached = torch.cuda.memory_cached(i) / 1024**3
            print(f"GPU {i} memory: allocated {allocated:.1f}GB, cached {cached:.1f}GB")


def main_optimized(args):
    """Main function (optimized for multi-CPU and multi-GPU performance)"""
    start_time = time.time()
    setup_seed(args.seed)
    
    # Set up parallel environment
    device, device_ids = setup_parallel_environment(args)
    
    print("=" * 60)
    print("StableCox Feature Selection (Optimized Parallel Version)")
    print("=" * 60)
    print(f"Using {args.num_workers} CPU cores")
    print(f"Using {len(device_ids) if device_ids else 0} GPUs")
    
    check_memory_usage()
    
    # 1. Load data
    print("1. Loading data...")
    df = pd.read_csv(args.data_csv)
    
    # Sample subset if specified
    original_size = len(df)
    if args.sample_size > 0 and args.sample_size < original_size:
        df = df.sample(n=args.sample_size, random_state=args.seed)
        print(f"  Data sampled: {args.sample_size:,} of {original_size:,} records")
    else:
        print(f"  Using full dataset: {original_size:,} records")
    
    duration_col = args.duration_col
    event_col = args.event_col

    # Check required columns
    for col in [duration_col, event_col]:
        if col not in df.columns:
            raise KeyError(f"Error: {col} not found in dataset!")

    # 2. Data preprocessing
    print("2. Data preprocessing...")
    X_df = df.drop([duration_col, event_col], axis=1)
    
    # Remove low-variance features
    low_var_cols = X_df.columns[X_df.var() < 1e-6].tolist()
    if low_var_cols:
        print(f"  Removing low-variance features: {len(low_var_cols)}")
        X_df = X_df.drop(columns=low_var_cols)
    
    # Use efficient data types
    X_df = X_df.astype(np.float32)
    
    # Standardize features
    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(X_df)
    X_df = pd.DataFrame(X_scaled, columns=X_df.columns, index=X_df.index)
    
    n, p = X_df.shape
    print(f"  Final dataset: {n:,} samples, {p} features")
    
    check_memory_usage()

    # 3. Sample reweighting (optimized version)
    print("3. Sample reweighting (optimized)...")
    if args.reweighting == "DWR":
        print("  Using optimized DWR method...")
        W = optimized_DWR(X_df.values, args, device, device_ids)
    elif args.reweighting == "SRDO":
        print("  Using SRDO method...")
        W = SRDO(X_df.values, p, decorrelation_type=args.decorrelation_type, 
                 max_iter=args.iters_balance)
    else:
        print("  Using equal weights...")
        W = np.ones((n, 1))

    # Postprocess weights
    W = np.clip(W, 1e-2, 10)
    W = W / W.mean()
    print("  Weight calculation complete")
    
    check_memory_usage()

    # 4. StableCox feature ranking
    print("4. StableCox feature ranking...")
    first_fit_df = X_df.copy()
    first_fit_df[duration_col] = df[duration_col]
    first_fit_df[event_col] = df[event_col]
    first_fit_df['Weights'] = W.flatten()

    # Fit Cox model using multiple threads
    cph_first = CoxPHFitter(penalizer=args.penalizer)
    cph_first.fit(first_fit_df, duration_col=duration_col, event_col=event_col, 
                 weights_col='Weights', robust=True, show_progress=True)
    
    # Extract significant features
    summary = cph_first.summary
    significant_features = summary[summary['p'] < args.p_threshold]
    significant_features_sorted = significant_features.sort_values('p')
    good_features = significant_features_sorted.index.tolist()
    
    print(f"  Identified {len(good_features)} significant features (p < {args.p_threshold})")

    # 5. Save results
    print("\n" + "=" * 50)
    print(f"Significant covariates selected by StableCox (p < {args.p_threshold}):")
    print("=" * 50)
    
    for i, feature in enumerate(good_features, 1):
        p_value = significant_features_sorted.loc[feature, 'p']
        coef = significant_features_sorted.loc[feature, 'coef']
        print(f"{i:2d}. {feature:40} coef: {coef:7.4f}  p: {p_value:.6f}")

    # Save outputs
    input_filename = os.path.basename(args.data_csv)
    filename_without_ext = os.path.splitext(input_filename)[0]
    
    feature_file = os.path.join(args.output_dir, f"{filename_without_ext}_significant_features.txt")
    with open(feature_file, 'w', encoding='utf-8') as f:
        for feature in good_features:
            f.write(f"{feature}\n")

    result_file = os.path.join(args.output_dir, f"{filename_without_ext}_detailed_results.csv")
    summary['significant'] = summary['p'] < args.p_threshold
    summary['significance_level'] = summary['p'].apply(
        lambda x: 'p < 0.001' if x < 0.001 else 
                 'p < 0.01' if x < 0.01 else 
                 'p < 0.05' if x < 0.05 else 'not significant'
    )
    all_features_sorted = summary.sort_values('p')
    all_features_sorted.to_csv(result_file)
    
    total_time = time.time() - start_time
    print(f"\n✅ StableCox feature selection completed in {total_time:.1f} seconds")
    print(f"📁 Significant features saved to: {feature_file}")
    print(f"📊 Detailed results saved to: {result_file}")


if __name__ == "__main__":
    args = get_args()
    main_optimized(args)
