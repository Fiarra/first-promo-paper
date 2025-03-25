import pdfplumber

def extract_text_from_pdf(pdf_path):
    """Extract all text from a PDF file using pdfplumber."""
    text = ""
    with pdfplumber.open(pdf_path) as pdf:
        for page in pdf.pages:
            text += page.extract_text() or ""  # fallback to empty string if no text
    return text
