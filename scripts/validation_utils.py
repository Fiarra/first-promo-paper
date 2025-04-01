"""
Validation utilities for the PDF extraction pipeline.
Provides functions for analyzing and visualizing extraction quality.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from typing import Dict, List, Tuple, Any, Optional

def add_validation_flags(df: pd.DataFrame) -> pd.DataFrame:
    """
    Add flags for potential extraction issues based on heuristics.
    
    Args:
        df: DataFrame with extracted title and abstract
        
    Returns:
        DataFrame with additional validation columns
    """
    # Make a copy to avoid modifying the original
    result_df = df.copy()
    
    # Flag 1: Missing or failed extractions
    result_df['title_missing'] = result_df['title'].str.contains('EXTRACTION_FAILED', case=False)
    result_df['abstract_missing'] = result_df['abstract'].str.contains('EXTRACTION_FAILED', case=False)
    
    # Flag 2: Suspiciously short titles (less than 5 characters)
    result_df['title_too_short'] = result_df['title'].str.len() < 5
    
    # Flag 3: Suspiciously short abstracts (less than 50 characters)
    result_df['abstract_too_short'] = result_df['abstract'].str.len() < 50
    
    # Flag 4: Suspiciously long titles (more than 200 characters)
    result_df['title_too_long'] = result_df['title'].str.len() > 200
    
    # Flag 5: Title contains typical error indicators
    error_patterns = r'error|not found|could not|unable to|failed|unavailable'
    result_df['title_error_pattern'] = result_df['title'].str.contains(error_patterns, case=False)
    
    # Calculate overall validation score (higher is better)
    # Start with 10 points and subtract for each flag
    result_df['validation_score'] = 10
    for flag_col in ['title_missing', 'abstract_missing', 'title_too_short', 
                      'abstract_too_short', 'title_too_long', 'title_error_pattern']:
        result_df['validation_score'] -= result_df[flag_col].astype(int) * 2
    
    # Ensure validation score is at least 0
    result_df['validation_score'] = result_df['validation_score'].clip(lower=0)
    
    return result_df

def plot_confidence_distribution(df: pd.DataFrame, config_name: str = "Current Config") -> None:
    """
    Plot histogram of confidence scores.
    
    Args:
        df: DataFrame with confidence scores
        config_name: Name of the configuration to display in the title
    """
    plt.figure(figsize=(12, 6))
    
    # Plot title confidence
    plt.subplot(1, 2, 1)
    plt.hist(df['title_confidence'], bins=5, range=(1, 6), alpha=0.7, color='blue')
    plt.title(f'Title Confidence ({config_name})')
    plt.xlabel('Confidence Score (1-5)')
    plt.ylabel('Number of Papers')
    plt.xticks([1, 2, 3, 4, 5])
    
    # Plot abstract confidence
    plt.subplot(1, 2, 2)
    plt.hist(df['abstract_confidence'], bins=5, range=(1, 6), alpha=0.7, color='green')
    plt.title(f'Abstract Confidence ({config_name})')
    plt.xlabel('Confidence Score (1-5)')
    plt.ylabel('Number of Papers')
    plt.xticks([1, 2, 3, 4, 5])
    
    plt.tight_layout()
    plt.show()

def compare_confidence_scores(df1: pd.DataFrame, df2: pd.DataFrame, 
                             label1: str = "Config 1", label2: str = "Config 2") -> None:
    """
    Compare confidence scores between two different configurations.
    
    Args:
        df1: DataFrame from first configuration
        df2: DataFrame from second configuration
        label1: Label for first configuration
        label2: Label for second configuration
    """
    plt.figure(figsize=(14, 6))
    
    # Plot title confidence comparison
    plt.subplot(1, 2, 1)
    plt.hist(df1['title_confidence'], bins=5, range=(1, 6), alpha=0.6, label=label1, color='blue')
    plt.hist(df2['title_confidence'], bins=5, range=(1, 6), alpha=0.6, label=label2, color='red')
    plt.title('Title Confidence Comparison')
    plt.xlabel('Confidence Score (1-5)')
    plt.ylabel('Number of Papers')
    plt.xticks([1, 2, 3, 4, 5])
    plt.legend()
    
    # Plot abstract confidence comparison
    plt.subplot(1, 2, 2)
    plt.hist(df1['abstract_confidence'], bins=5, range=(1, 6), alpha=0.6, label=label1, color='blue')
    plt.hist(df2['abstract_confidence'], bins=5, range=(1, 6), alpha=0.6, label=label2, color='red')
    plt.title('Abstract Confidence Comparison')
    plt.xlabel('Confidence Score (1-5)')
    plt.ylabel('Number of Papers')
    plt.xticks([1, 2, 3, 4, 5])
    plt.legend()
    
    plt.tight_layout()
    plt.show()

def generate_validation_report(df: pd.DataFrame, output_path: Optional[str] = None) -> Dict[str, Any]:
    """
    Generate a comprehensive validation report.
    
    Args:
        df: DataFrame with extraction results and confidence scores
        output_path: Optional path to save report as HTML
        
    Returns:
        Dictionary with validation statistics
    """
    # Apply validation flags
    validated_df = add_validation_flags(df)
    
    # Calculate statistics
    stats = {
        'total_papers': len(validated_df),
        'avg_title_confidence': validated_df['title_confidence'].mean(),
        'avg_abstract_confidence': validated_df['abstract_confidence'].mean(),
        'failed_title_extractions': validated_df['title_missing'].sum(),
        'failed_abstract_extractions': validated_df['abstract_missing'].sum(),
        'low_confidence_titles': (validated_df['title_confidence'] <= 2).sum(),
        'low_confidence_abstracts': (validated_df['abstract_confidence'] <= 2).sum(),
        'validation_flags': {
            'title_too_short': validated_df['title_too_short'].sum(),
            'title_too_long': validated_df['title_too_long'].sum(),
            'abstract_too_short': validated_df['abstract_too_short'].sum(),
            'title_error_pattern': validated_df['title_error_pattern'].sum()
        },
        'avg_validation_score': validated_df['validation_score'].mean()
    }
    
    # Print report
    print("====== VALIDATION REPORT ======")
    print(f"Total papers processed: {stats['total_papers']}")
    print(f"Average title confidence: {stats['avg_title_confidence']:.2f}/5.0")
    print(f"Average abstract confidence: {stats['avg_abstract_confidence']:.2f}/5.0")
    print(f"Failed title extractions: {stats['failed_title_extractions']} ({stats['failed_title_extractions']/stats['total_papers']*100:.1f}%)")
    print(f"Failed abstract extractions: {stats['failed_abstract_extractions']} ({stats['failed_abstract_extractions']/stats['total_papers']*100:.1f}%)")
    print("\nValidation flags:")
    for flag, count in stats['validation_flags'].items():
        print(f"  - {flag}: {count} ({count/stats['total_papers']*100:.1f}%)")
    print(f"\nAverage validation score: {stats['avg_validation_score']:.2f}/10.0")
    
    # Save report as HTML if output path is provided
    if output_path:
        # Create a styled HTML report
        html_report = f"""
        <html>
        <head>
            <style>
                body {{ font-family: Arial, sans-serif; margin: 20px; }}
                h1 {{ color: #2c3e50; }}
                h2 {{ color: #3498db; }}
                .stats {{ margin: 20px 0; }}
                .good {{ color: green; }}
                .warning {{ color: orange; }}
                .bad {{ color: red; }}
                table {{ border-collapse: collapse; width: 100%; }}
                th, td {{ border: 1px solid #ddd; padding: 8px; text-align: left; }}
                th {{ background-color: #f2f2f2; }}
                tr:nth-child(even) {{ background-color: #f9f9f9; }}
            </style>
        </head>
        <body>
            <h1>Extraction Validation Report</h1>
            
            <div class="stats">
                <h2>Overview</h2>
                <p>Total papers processed: <b>{stats['total_papers']}</b></p>
                <p>Average title confidence: <b class="{get_color_class(stats['avg_title_confidence'], 3.5, 2.5)}">{stats['avg_title_confidence']:.2f}/5.0</b></p>
                <p>Average abstract confidence: <b class="{get_color_class(stats['avg_abstract_confidence'], 3.5, 2.5)}">{stats['avg_abstract_confidence']:.2f}/5.0</b></p>
                <p>Average validation score: <b class="{get_color_class(stats['avg_validation_score'], 8, 6)}">{stats['avg_validation_score']:.2f}/10.0</b></p>
            </div>
            
            <div class="stats">
                <h2>Extraction Issues</h2>
                <p>Failed title extractions: <b class="{get_color_class(stats['failed_title_extractions'], 0, stats['total_papers']*0.1, inverse=True)}">{stats['failed_title_extractions']} ({stats['failed_title_extractions']/stats['total_papers']*100:.1f}%)</b></p>
                <p>Failed abstract extractions: <b class="{get_color_class(stats['failed_abstract_extractions'], 0, stats['total_papers']*0.1, inverse=True)}">{stats['failed_abstract_extractions']} ({stats['failed_abstract_extractions']/stats['total_papers']*100:.1f}%)</b></p>
                <p>Low confidence titles: <b class="{get_color_class(stats['low_confidence_titles'], 0, stats['total_papers']*0.1, inverse=True)}">{stats['low_confidence_titles']} ({stats['low_confidence_titles']/stats['total_papers']*100:.1f}%)</b></p>
                <p>Low confidence abstracts: <b class="{get_color_class(stats['low_confidence_abstracts'], 0, stats['total_papers']*0.1, inverse=True)}">{stats['low_confidence_abstracts']} ({stats['low_confidence_abstracts']/stats['total_papers']*100:.1f}%)</b></p>
            </div>
            
            <div class="stats">
                <h2>Validation Flags</h2>
                <table>
                    <tr>
                        <th>Flag</th>
                        <th>Count</th>
                        <th>Percentage</th>
                    </tr>
        """
        
        for flag, count in stats['validation_flags'].items():
            percentage = count/stats['total_papers']*100
            cls = get_color_class(count, 0, stats['total_papers']*0.1, inverse=True)
            html_report += f"""
                    <tr>
                        <td>{flag}</td>
                        <td>{count}</td>
                        <td class="{cls}">{percentage:.1f}%</td>
                    </tr>
            """
            
        html_report += """
                </table>
            </div>
            
            <div class="stats">
                <h2>Papers Requiring Review</h2>
                <p>The following papers should be manually reviewed due to low confidence or validation issues:</p>
                <table>
                    <tr>
                        <th>Document ID</th>
                        <th>Title</th>
                        <th>Title Confidence</th>
                        <th>Abstract Confidence</th>
                        <th>Validation Score</th>
                        <th>Issues</th>
                    </tr>
        """
        
        # Add papers that need review (low confidence or validation issues)
        papers_to_review = validated_df[
            (validated_df['title_confidence'] <= 3) | 
            (validated_df['abstract_confidence'] <= 3) |
            (validated_df['validation_score'] < 6)
        ]
        
        for _, row in papers_to_review.iterrows():
            issues = []
            for flag in ['title_missing', 'abstract_missing', 'title_too_short', 
                       'abstract_too_short', 'title_too_long', 'title_error_pattern']:
                if row[flag]:
                    issues.append(flag)
                    
            issues_str = ", ".join(issues)
            
            html_report += f"""
                    <tr>
                        <td>{row['document_id']}</td>
                        <td>{row['title'][:50] + "..." if len(row['title']) > 50 else row['title']}</td>
                        <td class="{get_color_class(row['title_confidence'], 3.5, 2.5)}">{row['title_confidence']:.1f}</td>
                        <td class="{get_color_class(row['abstract_confidence'], 3.5, 2.5)}">{row['abstract_confidence']:.1f}</td>
                        <td class="{get_color_class(row['validation_score'], 8, 6)}">{row['validation_score']}</td>
                        <td>{issues_str}</td>
                    </tr>
            """
        
        html_report += """
                </table>
            </div>
        </body>
        </html>
        """
        
        with open(output_path, 'w') as f:
            f.write(html_report)
        
        print(f"\nDetailed report saved to: {output_path}")
    
    return stats

def get_color_class(value, good_threshold, bad_threshold, inverse=False):
    """Helper function to determine color class for HTML report"""
    if inverse:
        if value <= good_threshold:
            return "good"
        elif value <= bad_threshold:
            return "warning"
        else:
            return "bad"
    else:
        if value >= good_threshold:
            return "good"
        elif value >= bad_threshold:
            return "warning"
        else:
            return "bad" 