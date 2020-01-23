#ifndef __CAP_CSVFILE_H__
#define __CAP_CSVFILE_H__

#include <stdlib.h>
#include <stdio.h>

typedef struct CSVRow {
    char** entries;
    int entryCount;

    struct CSVRow* prev;
    struct CSVRow* next;
} CSVRow;

typedef struct {
    const char* filepath;
    CSVRow* currentRow;
} CSVFile;

extern const CSVRow CSV_NULL_ROW;
extern CSVRow const* CSV_NULL_ROW_PTR;

CSVFile csvLoadFile(const char* filepath);

CSVRow const* csvNextRow(CSVFile* csvFile);
CSVRow const* csvPrevRow(CSVFile* csvFile);

CSVRow const* csvSeekEnd(CSVFile* csvFile);
CSVRow const* csvSeekBegin(CSVFile* csvFile);

void csvFreeRow(CSVRow* row);
void csvFreeFile(CSVFile* csvFile);

// stdio.h getline is POSIX, need portable version
int csvGetline(char** oLine, int* oLength, FILE* fp);

#endif /* __CAP_CSVFILE_H__ */