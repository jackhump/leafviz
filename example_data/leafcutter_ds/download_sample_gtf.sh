wget https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_43/gencode.v43.basic.annotation.gtf.gz
zcat gencode.v43.basic.annotation.gtf.gz | head -n 192500 | gzip - > gencode.v43.basic.annotation_sample.gtf.gz
