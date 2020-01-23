#include <stdio.h>
#include <stdlib.h>

#include "capstone/Matrix.h"
#include "capstone/Array.h"
#include "capstone/FileUtil.h"
#include "capstone/CSVFile.h"

int main(int argc, char** argv) {

    CSVFile csv = csvLoadFile("res/example.txt");

    for (CSVRow* row = csv.currentRow; row != CSV_NULL_ROW_PTR; row = csvNextRow(&csv)) {
        printf("row\n");
        for (int i = 0; i < row->entryCount; i++) {
            printf("\t%s\n", row->entries[i]);
        }    
    } 

    csvSeekBegin(&csv);

     for (CSVRow* row = csv.currentRow; row != CSV_NULL_ROW_PTR; row = csvNextRow(&csv)) {
        printf("row\n");
        for (int i = 0; i < row->entryCount; i++) {
            printf("\t%s\n", row->entries[i]);
        }    
    } 

    csvSeekEnd(&csv);
    
    for (CSVRow* row = csv.currentRow; row != CSV_NULL_ROW_PTR; row = csvNextRow(&csv)) {
        printf("row\n");
        for (int i = 0; i < row->entryCount; i++) {
            printf("\t%s\n", row->entries[i]);
        }    
    } 


    csvFreeFile(&csv);

    return 0;
}