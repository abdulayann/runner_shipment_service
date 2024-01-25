package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.opencsv.CSVReader;
import com.opencsv.exceptions.CsvException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.input.BOMInputStream;
import org.apache.commons.text.WordUtils;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
public class CSVParsingUtil<T> {

    private Class<T> entityClass;
    private final Set<String> hiddenFields = Set.of("pickupAddress",
            "deliveryAddress", "eventsList", "packsList", "shipmentsList", "bookingCharges");

    public CSVParsingUtil(Class<T> entityClass) {
        this.entityClass = entityClass;
    }

    public Set<String> generateContainerHeaders() {
        Set<String> hs = new LinkedHashSet<>();
        Field[] fields = Containers.class.getDeclaredFields();
        for (Field field : fields) {
            if (hiddenFields.contains(field.getName())) continue;
            hs.add(field.getName());
        }
        return hs;
    }

    public Set<String> generateCSVHeaderForPacking() {
        Set<String> hs = new LinkedHashSet<>();
        Field[] fields = Packing.class.getDeclaredFields();
        for (Field field : fields) {
            hs.add(field.getName());
        }
        return hs;
    }

    public Set<String> generateCSVHeaderForEvent() {
        Field[] fields = Events.class.getDeclaredFields();
        Set<String> hs = new LinkedHashSet<>();
        for (Field field : fields) {
            hs.add(field.getName());
        }
        return hs;
    }

    public List<String> getHeadersForShipment() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(ShipmentListResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("client", "consigner", "consignee", "additionalDetails", "carrierDetails", "pickupDetails", "deliveryDetails");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields) {
            headers.add(field.getName());
        }
        return headers;
    }


    public List<String> getHeadersForAllocations() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(AllocationsResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("id", "guid");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields) {
            headers.add(field.getName());
        }
        return headers;
    }

    public List<String> getHeadersForConsolidation() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(ConsolidationListResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("sendingAgent", "receivingAgent", "borrowedFrom", "creditor", "coLoadWith", "arrivalDetails",
                "departureDetails", "carrierDetails", "achievedQuantities", "allocations");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields)
            headers.add(field.getName());
        return headers;
    }

    public List<String> getHeadersForArrivalDepartureDetails() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(ConsolidationListResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("id", "guid", "containerYardId", "transportPortId", "CFSId", "CTOId", "firstForeignPortId", "lastForeignPortId");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields)
            headers.add(field.getName());
        return headers;
    }

    public List<String> getHeadersForParties() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(PartiesResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("guid", "id", "orgData", "addressData");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields) {
            headers.add(field.getName());
        }
        return headers;
    }

    public List<String> getHeadersForAchievedQuantities() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(AchievedQuantitiesResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("guid", "id");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields) {
            headers.add(field.getName());
        }
        return headers;
    }

    public List<String> getHeadersForCarrier() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(CarrierDetailResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("guid", "id", "masterData", "unlocationData", "carrierMasterData", "vesselsMasterData");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields) {
            headers.add(field.getName());
        }
        return headers;
    }

    public List<String> getHeadersForPDDetails() {
        List<String> headers = new ArrayList<>();
        List<Field> fields = Arrays.stream(PickupDeliveryDetailsListResponse.class.getDeclaredFields()).toList();
        Set removedFields = Set.of("guid", "id");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());
        for (Field field : fields) {
            headers.add(field.getName());
        }
        return headers;
    }

    public List<String> getHeadersForContainer() {
        List<String> headers = new ArrayList<>();
        List<Field> containerFields = Arrays.stream(ContainerResponse.class.getDeclaredFields()).toList();
        final Set requiredFields = Set.of("containerNumber", "volumeUtilization", "weightUtilization", "achievedVolume",
                "achievedVolumeUnit", "achievedWeight", "achievedWeightUnit", "grossVolume", "grossVolumeUnit",
                "allocatedWeight", "allocatedWeightUnit", "netWeight", "netWeightUnit", "grossWeight", "grossWeightUnit", "remarks",
                "extraParams", "chargeable", "chargeableUnit", "ownType", "packs", "packsType", "marksNums", "innerPackageMeasurementUnit", "pacrNumber");
        containerFields = containerFields.stream().filter(field -> {
            if (requiredFields.contains(field.getName())) return true;
            else return false;
        }).collect(Collectors.toList());

        for (Field field : containerFields) {
            headers.add(field.getName());
        }
        return headers;
    }

    public List<String> getAllAttributeValuesAsListForCarrier(CarrierDetailResponse carrierDetailResponse) throws IllegalAccessException {
        if (carrierDetailResponse == null) carrierDetailResponse = new CarrierDetailResponse();
        Class<?> clazz = carrierDetailResponse.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();

        Set removedFields = Set.of("guid", "id", "masterData", "unlocationData", "carrierMasterData", "vesselsMasterData");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = carrierDetailResponse == null ? null : field.get(carrierDetailResponse);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }

    public List<String> getAllAttributeValuesAsListForPDDetail(PickupDeliveryDetailsListResponse pdResponse) throws IllegalAccessException {
        if (pdResponse == null) pdResponse = new PickupDeliveryDetailsListResponse();
        Class<?> clazz = pdResponse.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();
        Set removedFields = Set.of("guid", "id");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = pdResponse == null ? null : field.get(pdResponse);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }

    public List<String> getAllAttributeValuesAsListForParty(PartiesResponse party) throws IllegalAccessException {
        if (party == null) party = new PartiesResponse();
        Class<?> clazz = party.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();
        Set removedFields = Set.of("guid", "id", "orgData", "addressData");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = party == null ? null : field.get(party);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }

    public List<String> getAllAttributeValuesAsList(ShipmentListResponse shipment) throws IllegalAccessException {
        if (shipment == null) shipment = new ShipmentListResponse();
        Class<?> clazz = shipment.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();
        Set removedFields = Set.of("client", "consigner", "consignee", "additionalDetails", "carrierDetails", "pickupDetails", "deliveryDetails");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = shipment == null ? null : field.get(shipment);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }

    public List<String> getAllAttributeValuesAsListConsol(ConsolidationListResponse response) throws IllegalAccessException {
        if (response == null) response = new ConsolidationListResponse();
        Class<?> clazz = response.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();
        Set removedFields = Set.of("sendingAgent", "receivingAgent", "borrowedFrom", "creditor", "coLoadWith", "arrivalDetails", "departureDetails", "carrierDetails", "achievedQuantities", "allocations"); // Parties
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = response == null ? null : field.get(response);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }

    public List<String> getAllAttributeValuesAsListForAllocations(AllocationsResponse response) throws IllegalAccessException {
        if (response == null) response = new AllocationsResponse();
        Class<?> clazz = response.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();
        Set removedFields = Set.of("id", "guid");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = response == null ? null : field.get(response);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }


    public List<String> getAllAttributeValuesAsListForAchievedQuantities(AchievedQuantitiesResponse response) throws IllegalAccessException {
        if (response == null) response = new AchievedQuantitiesResponse();
        Class<?> clazz = response.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();
        Set removedFields = Set.of("id", "guid");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = response == null ? null : field.get(response);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }

    public List<String> getAllAttributeValuesAsListForArrivalDepartureDetails(ArrivalDepartureDetailsResponse response) throws IllegalAccessException {
        if (response == null) response = new ArrivalDepartureDetailsResponse();
        Class<?> clazz = response.getClass();
        List<Field> fields = Arrays.stream(clazz.getDeclaredFields()).toList();
        Set removedFields = Set.of("id", "guid");
        fields = fields.stream().filter(field -> {
            if (removedFields.contains(field.getName())) return false;
            else return true;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : fields) {
            field.setAccessible(true);
            Object value = response == null ? null : field.get(response);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }

    public List<String> getAllAttributeValuesAsListContainer(ContainerResponse response) throws IllegalAccessException {
        Class<?> clazz = response.getClass();
        List<Field> containerFields = Arrays.stream(ContainerResponse.class.getDeclaredFields()).toList();
        final Set requiredFields = Set.of("containerNumber", "volumeUtilization", "weightUtilization", "achievedVolume",
                "achievedVolumeUnit", "achievedWeight", "achievedWeightUnit", "grossVolume", "grossVolumeUnit",
                "allocatedWeight", "allocatedWeightUnit", "netWeight", "netWeightUnit", "grossWeight", "grossWeightUnit", "remarks",
                "extraParams", "chargeable", "chargeableUnit", "ownType", "packs", "packsType", "marksNums", "innerPackageMeasurementUnit", "pacrNumber");
        containerFields = containerFields.stream().filter(field -> {
            if (requiredFields.contains(field.getName())) return true;
            else return false;
        }).collect(Collectors.toList());

        List<String> lst = new ArrayList<>();

        for (Field field : containerFields) {
            field.setAccessible(true);
            Object value = response == null ? null : field.get(response);
            lst.add(value != null ? value.toString() : "");
        }
        return lst;
    }

    public void addContainerToSheet(T entity, XSSFWorkbook workbook, XSSFSheet sheet, int rowNum) {
        Row row = sheet.createRow(rowNum);
        Field[] fields = Containers.class.getDeclaredFields();
        int counter = 0;
        addRowToSheet(entity, row, fields, counter);
    }

    public void addPackToSheet(T entity, XSSFWorkbook workbook, XSSFSheet sheet, int rowNum) {
        Row row = sheet.createRow(rowNum);
        Field[] fields = Packing.class.getDeclaredFields();
        int counter = 0;
        addRowToSheet(entity, row, fields, counter);
    }

    private void addRowToSheet(T entity, Row row, Field[] fields, int counter) {
        for (Field field : fields) {
            if (hiddenFields.contains(field.getName())) continue;
            field.setAccessible(true);
            try {
                Object value = field.get(entity);
                row.createCell(counter++).setCellValue(value != null ? value.toString() : "");
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            }
        }
    }

    public void addEventToSheet(Events entity, XSSFWorkbook workbook, XSSFSheet sheet, int rowNum) {
        Field[] fields = Events.class.getDeclaredFields();
        Row row = sheet.createRow(rowNum);
        int counter = 0;
        for (Field field : fields) {
            if (hiddenFields.contains(field.getName())) continue;
            field.setAccessible(true);
            try {
                Object value = field.get(entity);
                row.createCell(counter++).setCellValue(value != null ? value.toString() : "");
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            }
        }
    }


    public String getCamelCase(String name) {
        return WordUtils.uncapitalize(name);
    }

    private T createEntityInstance() throws InstantiationException, IllegalAccessException {
        return entityClass.newInstance();
    }

    public List<T> parseCSVFile(MultipartFile file) throws IOException, CsvException {
        List<T> entityList = new ArrayList<>();
        try (CSVReader csvReader = new CSVReader(new InputStreamReader(new BOMInputStream(file.getInputStream()), StandardCharsets.UTF_8))) {
            String[] header = csvReader.readNext();
            for (int i = 0; i < header.length; i++) {
                header[i] = getCamelCase(header[i]);
            }

            List<String[]> records = csvReader.readAll();
            for (String[] record : records) {
                T entity = createEntityInstance();
                for (int i = 0; i < record.length; i++) {
                    setField(entity, header[i], record[i]);
                }
                entityList.add(entity);
            }
        } catch (NoSuchFieldException | IllegalAccessException | InstantiationException e) {
            throw new RuntimeException(e);
        }
        return entityList;
    }

    public List<T> parseExcelFile(MultipartFile file) throws IOException {
        List<T> entityList = new ArrayList<>();
        try (Workbook workbook = new XSSFWorkbook(file.getInputStream())) {
            Sheet sheet = workbook.getSheetAt(0); // Assuming data is in the first sheet
            validateExcel(sheet);
            Row headerRow = sheet.getRow(0);
            String[] header = new String[headerRow.getLastCellNum()];
            Set<String> headerSet = new HashSet<>();
            for (int i = 0; i < headerRow.getLastCellNum(); i++) {
                if(StringUtility.isEmpty(headerRow.getCell(i).getStringCellValue())) {
                    continue;
                }
                header[i] = getCamelCase(headerRow.getCell(i).getStringCellValue());
                headerSet.add(header[i]);
            }

            if(headerSet.size() < headerRow.getLastCellNum()) {
                throw new ValidationException("Excel Sheet is invalid. All column should have column name.");
            }


            for (int i = 1; i <= sheet.getLastRowNum(); i++) {
                Row row = sheet.getRow(i);
                T entity = createEntityInstance();

                for (int j = 0; j < header.length; j++) {
                    Cell cell = row.getCell(j);
                    if (cell != null) {
                        String cellValue = getCellValueAsString(cell);
                        setField(entity, header[j], cellValue);
                    }
                }

                entityList.add(entity);
            }
        }  catch (ValidationException e1) {
            log.error(e1.getMessage());
            throw new ValidationException(e1.getMessage());
        } catch (NoSuchFieldException | IllegalAccessException | InstantiationException e) {
            log.error(e.getMessage());
            throw new ValidationException("Wxcel sheet is not valid.");
        }
        return entityList;
    }

    private String getCellValueAsString(Cell cell) {
        switch (cell.getCellType()) {
            case STRING:
                return cell.getStringCellValue();
            case NUMERIC:
                if (DateUtil.isCellDateFormatted(cell)) {
                    return cell.getLocalDateTimeCellValue().toString();
                } else {
                    if (cell.getNumericCellValue() == Math.floor(cell.getNumericCellValue())) {
                        return String.valueOf((long) cell.getNumericCellValue());
                    } else {
                        return String.valueOf(cell.getNumericCellValue());
                    }
                }
            case BOOLEAN:
                return String.valueOf(cell.getBooleanCellValue());
            case FORMULA:
                return cell.getCellFormula();
            default:
                return "";
        }
    }

    private void validateExcel(Sheet sheet)
    {

        //check excel sheet has rows (empty excelsheet uploaded)
        if (sheet == null || sheet.getLastRowNum() <= 0)
        {
            throw new ValidationException("Empty excel sheet uploaded.");
        }
        //check rows are more than or equal 2 (excel sheet has only header row)
        else if (sheet.getLastRowNum() <= 1)
        {
            throw new ValidationException("Excel sheet does not contain any data.");
        }
    }


    public List<Events> parseCSVFileEvents(MultipartFile file) throws IOException, CsvException {
        List<Events> entityList = new ArrayList<>();
        try (CSVReader csvReader = new CSVReader(new InputStreamReader(new BOMInputStream(file.getInputStream()), StandardCharsets.UTF_8))) {
            String[] header = csvReader.readNext();
            for (int i = 0; i < header.length; i++) {
                header[i] = getCamelCase(header[i]);
            }

            List<String[]> records = csvReader.readAll();
            for (String[] record : records) {
                Events entity = new Events();
                for (int i = 0; i < record.length; i++) {
                    setFieldForEvents(entity, header[i], record[i]);
                }
                entityList.add(entity);
            }
        } catch (NoSuchFieldException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
        return entityList;
    }


    private void setField(T entity, String attributeName, String attributeValue) throws NoSuchFieldException, IllegalAccessException {
        Field field = entity.getClass().getDeclaredField(attributeName);
        field.setAccessible(true);

        Class<?> fieldType = field.getType();
        Object parsedValue = null;
        if (attributeValue.isEmpty())
            return;
        if (fieldType == int.class || fieldType == Integer.class) {
            parsedValue = Integer.parseInt(attributeValue);
        } else if (fieldType == String.class) {
            parsedValue = attributeValue;
        } else if (fieldType == Long.class || fieldType == long.class) {
            parsedValue = Long.parseLong(attributeValue);
        } else if (fieldType == boolean.class || fieldType == Boolean.class) {
            parsedValue = Boolean.parseBoolean(attributeValue);
        } else if (fieldType == BigDecimal.class) {
            parsedValue = BigDecimal.valueOf(Double.valueOf(attributeValue));
        } else if (fieldType == ContainerStatus.class) {
            parsedValue = ContainerStatus.valueOf(attributeValue);
        } else if (fieldType == LocalDateTime.class) {
            parsedValue = LocalDateTime.parse(attributeValue);
        } else {
            throw new NoSuchFieldException();
        }

        field.set(entity, parsedValue);
    }

    public void setFieldForEvents(Events entity, String attributeName, String attributeValue) throws NoSuchFieldException, IllegalAccessException {
        Field field = entity.getClass().getDeclaredField(attributeName);
        field.setAccessible(true);

        Class<?> fieldType = field.getType();
        Object parsedValue = null;
        if (attributeValue.isEmpty())
            return;
        if (fieldType == int.class || fieldType == Integer.class) {
            parsedValue = Integer.parseInt(attributeValue);
        } else if (fieldType == String.class) {
            parsedValue = attributeValue;
        } else if (fieldType == Long.class || fieldType == long.class) {
            parsedValue = Long.parseLong(attributeValue);
        } else if (fieldType == boolean.class || fieldType == Boolean.class) {
            parsedValue = Boolean.parseBoolean(attributeValue);
        } else if (fieldType == BigDecimal.class) {
            parsedValue = BigDecimal.valueOf(Double.valueOf(attributeValue));
        } else if (fieldType == ContainerStatus.class) {
            parsedValue = ContainerStatus.valueOf(attributeValue);
        } else if (fieldType == LocalDateTime.class) {
            parsedValue = LocalDateTime.parse(attributeValue);
        } else {
            throw new NoSuchFieldException();
        }

        field.set(entity, parsedValue);
    }

}
