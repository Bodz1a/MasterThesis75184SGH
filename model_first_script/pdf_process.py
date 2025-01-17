import sys
import pdfplumber


def pdf_to_text(pdf_path):
    extracted_text = []

    with pdfplumber.open(pdf_path) as pdf:
        for page_number, page in enumerate(pdf.pages, start=1):
            text = page.extract_text()
            if text:
                extracted_text.append(f"=== Page number {page_number} ===\n{text}\n")
            else:
                extracted_text.append(f"=== Page number {page_number} ===\n(No selectable text)\n")

    return "\n".join(extracted_text)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python pdf_process.py <pdf_path>")
        sys.exit(1)

    pdf_file = sys.argv[1]
    result = pdf_to_text(pdf_file)
    print(result)
