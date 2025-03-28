package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;

import java.io.File;
import java.io.FileOutputStream;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

@Slf4j
@Component
public class ExcelUtils {
    public void createCell(XSSFSheet sheet, Row row, int columnCount, Object value, CellStyle style) {
        sheet.autoSizeColumn(columnCount);
        Cell cell = row.createCell(columnCount);
        if (value instanceof Integer) {
            cell.setCellValue((Integer) value);
        } else if (value instanceof Boolean) {
            cell.setCellValue((Boolean) value);
        } else if (value instanceof Date) {
            SimpleDateFormat formatter = new SimpleDateFormat("MM/dd/yyyy hh:mm:ss a");
            String strDate = formatter.format(value);
            cell.setCellValue(strDate);
        } else if (value instanceof BigDecimal) {
            cell.setCellValue(value.toString());
        } else if (value instanceof ArrayList<?>) {
            List<?> values = (List<?>) value;
            String arrayListAsString = values.stream()
                    .map(Object::toString)
                    .collect(Collectors.joining(", "));
            cell.setCellValue(arrayListAsString);
        } else {
            cell.setCellValue(value.toString());
        }
        cell.setCellStyle(style);
    }

    public String writeWorkbook(Workbook workbook) {
        String tempFileName = "";
        try {
            tempFileName = System.currentTimeMillis() + Constants.XLSX;
            FileOutputStream fileOutputStream = new FileOutputStream(tempFileName);
            workbook.write(fileOutputStream);
            workbook.close();
            fileOutputStream.close();
        } catch (Exception e) {
            log.error(e.getMessage());
        }
        return tempFileName;
    }

    public void writeHeader(XSSFWorkbook workbook, Set<String> columnHeaders, XSSFSheet sheet) {
        Row row = sheet.createRow(0);
        CellStyle style = workbook.createCellStyle();
        Font font = workbook.createFont();
        font.setBold(true);
        font.setFontHeight(Short.valueOf("16"));
        style.setFont(font);
        AtomicReference<Integer> columnCount = new AtomicReference<>(-1);
        Row firstRow = row;
        CellStyle headerStyle = style;
        columnHeaders.forEach(key -> {
            columnCount.updateAndGet(v -> v + 1);
            createCell(sheet, firstRow, columnCount.get(), key, headerStyle);
        });
    }

    public void writeSimpleHeader(Set<String> columnHeaders, XSSFSheet sheet) {
        Row row = sheet.createRow(0);
        int i = 0;
        for (var c : columnHeaders) {
            row.createCell(i++).setCellValue(c);
        }
    }

    public XSSFSheet createSheet(XSSFWorkbook workbook, String sheetName) {
        XSSFSheet sheet;
        sheet = workbook.createSheet(sheetName);
        return sheet;
    }

    public void writeDataToSheet(XSSFWorkbook workbook, XSSFSheet sheet, List<Map<String, Object>> dataMap,
                                 Map<String, String> excelFieldsMap) {
        CellStyle style = workbook.createCellStyle();
        Font font = workbook.createFont();
        font.setFontHeight(Short.valueOf("14"));
        style.setFont(font);
        Integer rowCount = 0;
        AtomicReference<Integer> columnCount = new AtomicReference<>(-1);

        for (Map<String, Object> map : dataMap) {
            rowCount = rowCount + 1;
            Row row = sheet.createRow(rowCount);
            columnCount.updateAndGet(v -> -1);
            // column config
            for (String key : excelFieldsMap.keySet()) {
                columnCount.updateAndGet(v -> v + 1);
                createCell(sheet, row, columnCount.get(), map.get(excelFieldsMap.get(key)), style);
            }
        }
    }

    public XSSFWorkbook createWorkBook() {
        return new XSSFWorkbook();
    }

    public Resource getResource(XSSFWorkbook workbook) throws RunnerException {
        Resource fileResource;
        try {
            String tempFile = writeWorkbook(workbook);
            File file = new File(tempFile);
            byte[] fileDataBytes = Files.readAllBytes(Paths.get(file.getPath()));
            fileResource = new ByteArrayResource(fileDataBytes);
            file.delete();
        } catch (Exception ex) {
            log.error("Unable to process the file");
            throw new RunnerException("Unable to process the file.");
        }

        return fileResource;
    }

    public Resource createExcelAsResource(List<Map<String, Object>> listAsMap, Map<String, String> columnHeadersToFieldName, String sheetName) throws RunnerException {
        XSSFWorkbook workbook = createWorkBook();
        XSSFSheet sheet = createSheet(workbook, sheetName);
        writeHeader(workbook, columnHeadersToFieldName.keySet(), sheet);
        writeDataToSheet(workbook, sheet, listAsMap, columnHeadersToFieldName);
        Resource fileResource = getResource(workbook);
        return fileResource;
    }

}
