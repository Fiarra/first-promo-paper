def display_summary_statistics(df):
    """
    Display summary statistics about the extracted titles and abstracts.
    
    Args:
        df: DataFrame containing the extracted data
    """
    # Display simple statistics about the extraction
    print(f"Total papers processed: {len(df)}")
    
    # Count papers with valid titles (not containing error messages)
    title_pattern = r'ERROR|I dont know'
    abstract_pattern = r'ERROR|I dont know'
    
    valid_titles = df['title'].count() - df['title'].str.contains(title_pattern).sum()
    valid_abstracts = df['abstract'].count() - df['abstract'].str.contains(abstract_pattern).sum()
    
    print(f"Papers with title extracted: {valid_titles}")
    print(f"Papers with abstract extracted: {valid_abstracts}")
    
    # Calculate average lengths
    avg_title_length = df['title'].str.len().mean()
    avg_abstract_length = df['abstract'].str.len().mean()
    print(f"Average title length: {avg_title_length:.1f} characters")
    print(f"Average abstract length: {avg_abstract_length:.1f} characters")
    
    return {
        "total_papers": len(df),
        "valid_titles": valid_titles,
        "valid_abstracts": valid_abstracts,
        "avg_title_length": avg_title_length,
        "avg_abstract_length": avg_abstract_length
    } 