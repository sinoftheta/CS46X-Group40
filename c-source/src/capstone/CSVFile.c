#include "CSVFile.h"

#include "../capstone/CSVFile.h"
#include "../capstone/FileUtil.h"

#include <string.h>

CSVRow CSV_NULL_ROW = {
    NULL, -1, &CSV_NULL_ROW, &CSV_NULL_ROW
};

CSVRow* CSV_NULL_ROW_PTR = &CSV_NULL_ROW;



CSVFile csvLoadFile(const char* filepath) {

    CSVFile result;
    result.filepath = filepath;
    result.currentRow = CSV_NULL_ROW_PTR;

    FILE* fp;
    fp = fopen(filepath, "r");

    char* line = NULL;
    int length = 0;

    while (readline(&line, &length, fp) != -1) {
        CSVRow* row = calloc(1, sizeof(CSVRow));
        row->prev = CSV_NULL_ROW_PTR;
        row->next = CSV_NULL_ROW_PTR;
        row->entries = csvTokenizeLine(line, &(row->entryCount));

        row->prev = result.currentRow;
        result.currentRow->next = row;
        result.currentRow = result.currentRow->next;
        
        free(line);
        line = NULL;
        length = 0;
    }

    fclose(fp);

    csvSeekBegin(&result);

    return result;
}

CSVRow* csvNextRow(CSVFile* csvFile) {
    if (csvFile->currentRow == CSV_NULL_ROW_PTR || csvFile->currentRow->next == CSV_NULL_ROW_PTR)
        return CSV_NULL_ROW_PTR;
    
    csvFile->currentRow = csvFile->currentRow->next;
    return csvFile->currentRow;
}

CSVRow* csvPrevRow(CSVFile* csvFile) {
    if (csvFile->currentRow == CSV_NULL_ROW_PTR || csvFile->currentRow->prev == CSV_NULL_ROW_PTR)
        return CSV_NULL_ROW_PTR;
    
    csvFile->currentRow = csvFile->currentRow->prev;
    return csvFile->currentRow;
}

CSVRow* csvSeekEnd(CSVFile* csvFile) {
    while (csvNextRow(csvFile) != CSV_NULL_ROW_PTR);
    return csvFile->currentRow;
}

CSVRow* csvSeekBegin(CSVFile* csvFile) {
    while (csvPrevRow(csvFile) != CSV_NULL_ROW_PTR);
    return csvFile->currentRow;
}

void csvFreeRow(CSVRow* row) {
    for (int i = 0; i < row->entryCount; i++) {
        free(row->entries[i]);
        row->entries[i] = NULL;
    }
    free(row->entries);
    row->entries = 0;

    // let someone else free row itself; 
    // row should be owned by a CSVFile
}

void csvFreeFile(CSVFile* csvFile) {
    CSVRow* row = csvSeekBegin(csvFile);

    while(row != CSV_NULL_ROW_PTR) {
        
        csvFreeRow(row);

        CSVRow* next = row->next;
        free(row);
        row = next;
    }

    // let someone else free csvFile
    // likely that csvFile is not on the heap
}

char** csvTokenizeLine(char* linebase, int* tokenCount) {

    if (linebase == NULL)
        return NULL;

    char* line = linebase;

    // expect at least 2 tokens
    if (*tokenCount <= 0)
        *tokenCount = 2;

    char** result = calloc(*tokenCount, sizeof(char*)); 

    if (result == NULL) 
        return NULL;

    int actualTokenCount = 0;
    int currentTokenLength = 0;
    do {
        do {
            currentTokenLength++;
            line++;
        } while (*line != ',' && *line != '\n');

        char* token = calloc(currentTokenLength + 1, sizeof(char));
        memcpy(token, line - currentTokenLength, currentTokenLength);
        result[actualTokenCount] = token;

        actualTokenCount++;
        if (actualTokenCount >= *tokenCount) {
            *tokenCount += 5;
            result = realloc(result, *tokenCount * sizeof(char*));
        }

        // some what hacky, but this asserts that the comma is not included 
        // on succesive iterations
        currentTokenLength = -1;

    } while (*line != '\n');

    *tokenCount = actualTokenCount;
    return result;
}