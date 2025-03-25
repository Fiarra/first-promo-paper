import os

def rename_pdfs_in_folder(folder_path, prefix="paper"):
    """
    Renames all PDFs in a folder to paper_001.pdf, paper_002.pdf, ...
    Returns list of full paths to renamed files.
    """
    pdf_files = sorted([f for f in os.listdir(folder_path) if f.lower().endswith(".pdf")])
    renamed_files = []

    for i, filename in enumerate(pdf_files, start=1):
        new_name = f"{prefix}_{i:03}.pdf"
        old_path = os.path.join(folder_path, filename)
        new_path = os.path.join(folder_path, new_name)
        os.rename(old_path, new_path)
        renamed_files.append(new_path)
    
    return renamed_files