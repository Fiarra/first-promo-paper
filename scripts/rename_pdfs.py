import os
from scripts.config_loader import config

def rename_pdfs_in_folder(folder_path=None, prefix=None):
    """
    Renames all PDFs in a folder to a standardized naming format.
    
    Args:
        folder_path: Directory containing PDFs (default from config)
        prefix: Prefix for renamed files (default from config)
        
    Returns:
        Tuple of (renamed_files, original_filenames_map) where:
        - renamed_files is a list of full paths to renamed files
        - original_filenames_map is a dict mapping new filenames to original filenames
    """
    # Use arguments or fall back to config values
    if folder_path is None:
        folder_path = config.get("paths.data_dir")
    
    if prefix is None:
        prefix = config.get("pdf.rename.prefix", "paper")
    
    # Dictionary to map new filenames to original filenames
    original_filenames_map = {}
    
    # Check if renaming is enabled in config
    if not config.get("pdf.rename.enabled", True):
        print("PDF renaming is disabled in config. Returning original filenames.")
        pdf_files = sorted([os.path.join(folder_path, f) for f in os.listdir(folder_path) 
                           if f.lower().endswith(".pdf")])
        # Create mapping of file to itself since no renaming occurs
        for file_path in pdf_files:
            filename = os.path.basename(file_path)
            original_filenames_map[filename] = filename
        return pdf_files, original_filenames_map
    
    try:
        pdf_files = sorted([f for f in os.listdir(folder_path) if f.lower().endswith(".pdf")])
        renamed_files = []

        for i, filename in enumerate(pdf_files, start=1):
            new_name = f"{prefix}_{i:03}.pdf"
            old_path = os.path.join(folder_path, filename)
            new_path = os.path.join(folder_path, new_name)
            
            # Store the mapping of new filename to original filename
            original_filenames_map[new_name] = filename
            
            # Only rename if the file doesn't already have the correct name
            if filename != new_name:
                os.rename(old_path, new_path)
            
            renamed_files.append(new_path)
        
        return renamed_files, original_filenames_map
    
    except Exception as e:
        print(f"Error renaming PDFs in {folder_path}: {str(e)}")
        return [], {}