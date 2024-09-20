package com.dpw.runner.shipment.services.utils;

import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.core.io.Resource;

import java.math.BigDecimal;
import java.util.*;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class ExcelUtilsTest {
    @InjectMocks
    private ExcelUtils excelUtils;

    private Map<String, Object> testDataMap;

    @BeforeEach
    void setUp() {
        testDataMap = new HashMap<>();
        testDataMap.put("val5", "value1");
        testDataMap.put("val6", 123);
        testDataMap.put("val1", true);
        testDataMap.put("val2", new Date());

        ArrayList<String> arrayList = new ArrayList<>();
        arrayList.add("index1");
        arrayList.add("index2");

        testDataMap.put("val3", arrayList);
        testDataMap.put("val4", BigDecimal.ONE);
    }

    @Test
    void testCreateExcelAsResource() throws Exception {
        Map<String, String> hm = Map.of("key1", "val1", "key2", "val2" , "key4" , "val4" , "key5", "val5", "key6", "val6" , "key3" , "val3");
        Resource result = excelUtils.createExcelAsResource(List.of(testDataMap), hm, "SheetName");
        assertNotNull(result);
    }

    @Test
    void testWriteSimpleHeader() {
        Set<String> columnHeaders = new HashSet<>();
        columnHeaders.add("Name");
        columnHeaders.add("Age");
        columnHeaders.add("Gender");

        XSSFWorkbook workbook = new XSSFWorkbook();

        XSSFSheet sheet = workbook.createSheet("MySheet");
        excelUtils.writeSimpleHeader(workbook, columnHeaders, sheet);
        assertNotNull(sheet);
    }
}
