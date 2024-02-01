package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.response.V1ContainerTypeResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.CommodityResponse;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import com.opencsv.CSVReader;
import com.opencsv.exceptions.CsvException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.input.BOMInputStream;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.text.WordUtils;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Slf4j
@Component
public class CSVParsingUtil<T> {

    private Class<T> entityClass;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private JsonHelper jsonHelper;
    private final Set<String> hiddenFields = Set.of("pickupAddress",
            "deliveryAddress", "eventsList", "packsList", "shipmentsList", "bookingCharges");
    ExecutorService executorService = Executors.newFixedThreadPool(10);


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

    public Map<Integer, Map<String, MasterData>> fetchInBulkMasterList(MasterListRequestV2 requests) {
        Map<String, MasterData> keyMasterDataMap = new HashMap<>();
        Map<Integer, Map<String, MasterData>> dataMap = new HashMap<>();
        if (requests.getMasterListRequests() != null && requests.getMasterListRequests().size() > 0) {
            V1DataResponse response = v1Service.fetchMultipleMasterData(requests);
            List<MasterData> masterLists = jsonHelper.convertValueToList(response.entities, MasterData.class);
            masterLists.forEach(masterData -> {
                dataMap.putIfAbsent(masterData.getItemType(), new HashMap<>());
                dataMap.get(masterData.getItemType()).put(masterData.getItemValue(), masterData);
            });
        }
        return dataMap;
    }


    public List<T> parseExcelFile(MultipartFile file, BulkUploadRequest request, Map<UUID, T> mapOfEntity) throws IOException {
        List<T> entityList = new ArrayList<>();
        List<String> unlocationsList = new ArrayList<>();
        List<String> commodityCodesList = new ArrayList<>();

        int containerStuffingLocationPos = -1;
        int commodityCodePos = -1;
        int guidPos = -1;
        
        try (Workbook workbook = new XSSFWorkbook(file.getInputStream())) {
            Sheet sheet = workbook.getSheetAt(0); // Assuming data is in the first sheet
            validateExcel(sheet);
            Row headerRow = sheet.getRow(0);
            if (headerRow.getLastCellNum() <= 1) {
                throw new ValidationException("Empty excel sheet uploaded.");
            }
            String[] header = new String[headerRow.getLastCellNum()];
            Set<String> headerSet = new HashSet<>();
            for (int i = 0; i < headerRow.getLastCellNum(); i++) {
                if (StringUtility.isEmpty(headerRow.getCell(i).getStringCellValue())) {
                    continue;
                }
                header[i] = getCamelCase(headerRow.getCell(i).getStringCellValue());
                headerSet.add(header[i]);
                if (header[i].equalsIgnoreCase("guid")) {
                    guidPos = i;
                }
                if (header[i].equalsIgnoreCase("containerStuffingLocation")) {
                    containerStuffingLocationPos = i;
                }
                if (header[i].equalsIgnoreCase("commodityCode")) {
                    commodityCodePos = i;
                }
            }

            //fetching master data in bulk
            Map<String, Set<String>> masterListsMap = getAllMasterData(unlocationsList, commodityCodesList);

            if (headerSet.size() < headerRow.getLastCellNum()) {
                throw new ValidationException("Excel Sheet is invalid. All column should have column name.");
            }
            Set<String> guidSet = new HashSet<>();
            for (int i = 1; i <= sheet.getLastRowNum(); i++) {
                Row row = sheet.getRow(i);
                if (commodityCodePos != -1) {
                    String commodityCode = getCellValueAsString(row.getCell(commodityCodePos));
                    if (!StringUtils.isEmpty(commodityCode))
                        commodityCodesList.add(commodityCode);
                }
                if (containerStuffingLocationPos != -1) {
                    String unloc = getCellValueAsString(row.getCell(containerStuffingLocationPos));
                    if (!StringUtils.isEmpty(unloc))
                        unlocationsList.add(unloc);
                }
                if (guidPos != -1) {
                    if (guidSet.contains(getCellValueAsString(row.getCell(guidPos)))) {
                        throw new ValidationException("GUID is duplicate at row: " + row);
                    }
                    guidSet.add(getCellValueAsString(row.getCell(guidPos)));
                }
            }

            for (int i = 1; i <= sheet.getLastRowNum(); i++) {
                Row row = sheet.getRow(i);
                if (guidPos != -1) {
                    String guidVal = getCellValueAsString(row.getCell(guidPos));
                    try {
                        var containerGuid = UUID.fromString(guidVal);
                        if (mapOfEntity != null && !mapOfEntity.containsKey(containerGuid)) {
                            throw new ValidationException("GUID at row: " + row + " doesn't exist for this consolidation.");
                        }
                    } catch (Exception ex) {
                        throw new ValidationException("GUID not valid at row: " + row);
                    }
                }
                T entity = guidPos != -1 && mapOfEntity != null ? mapOfEntity.get(row.getCell(guidPos)) : createEntityInstance();

                for (int j = 0; j < header.length; j++) {
                    Cell cell = row.getCell(j);
                    if (cell != null) {
                        String cellValue = getCellValueAsString(cell);
                        checkForUnitValidations(masterListsMap, header[j], cellValue, i, request.getTransportMode());
                        checkForValueValidations(header[j], cellValue, i, request.getTransportMode());
                        setField(entity, header[j], cellValue);
                    }
                }

                entityList.add(entity);
            }
        } catch (ValidationException e1) {
            log.error(e1.getMessage());
            throw new ValidationException(e1.getMessage());
        } catch (NoSuchFieldException | IllegalAccessException | InstantiationException e) {
            log.error(e.getMessage());
            throw new ValidationException("Excel sheet is not valid.");
        }
        return entityList;
    }

    public VolumeWeightChargeable calculateVolumeWeight(String transportMode, String weightUnit, String volumeUnit, BigDecimal weight, BigDecimal volume) throws Exception {
        String responseMsg;
        try {
            VolumeWeightChargeable vwOb = new VolumeWeightChargeable();
            if (!weightUnit.isEmpty() && !volumeUnit.isEmpty() && !transportMode.isEmpty()) {
                switch (transportMode) {
                    case Constants.TRANSPORT_MODE_SEA:
                    case Constants.TRANSPORT_MODE_RAI:
                    case Constants.TRANSPORT_MODE_FSA:
                        BigDecimal volInM3 = new BigDecimal(convertUnit(Constants.VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());
                        vwOb.setChargeable(volInM3.multiply(BigDecimal.valueOf(10)).setScale(0, BigDecimal.ROUND_CEILING).divide(BigDecimal.valueOf(10)));
                        vwOb.setChargeableUnit(Constants.VOLUME_UNIT_M3);

                        BigDecimal wtInTn = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
                        wtInTn = wtInTn.divide(BigDecimal.valueOf(1000));
                        BigDecimal wv = new BigDecimal(convertUnit(Constants.VOLUME, wtInTn, Constants.VOLUME_UNIT_M3, volumeUnit).toString());
                        vwOb.setVolumeWeight(wv.multiply(BigDecimal.TEN).setScale(0, RoundingMode.CEILING).divide(BigDecimal.TEN));
                        vwOb.setVolumeWeightUnit(volumeUnit);
                        break;
                    case Constants.TRANSPORT_MODE_AIR:
                    case Constants.TRANSPORT_MODE_FAS:
                    case Constants.TRANSPORT_MODE_ROA:
                        BigDecimal wtInKG = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
                        BigDecimal vlInM3 = new BigDecimal(convertUnit(Constants.VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());
                        BigDecimal factor = new BigDecimal(166.667);
                        if (transportMode.equals(Constants.TRANSPORT_MODE_ROA)) {
                            factor = BigDecimal.valueOf(333.0);
                        }
                        BigDecimal wvInKG = vlInM3.multiply(factor);
                        if (wtInKG.compareTo(wvInKG) < 0) {
                            wtInKG = wvInKG;
                        }
                        vwOb.setChargeable(wtInKG.multiply(BigDecimal.valueOf(100)).setScale(0, BigDecimal.ROUND_CEILING).divide(BigDecimal.valueOf(100)));
                        vwOb.setChargeableUnit(Constants.WEIGHT_UNIT_KG);
                        BigDecimal WV = new BigDecimal(convertUnit(Constants.MASS, wvInKG, Constants.WEIGHT_UNIT_KG, weightUnit).toString());
                        vwOb.setVolumeWeight(WV);
                        vwOb.setVolumeWeightUnit(weightUnit);
                        break;
                }
            }
            return vwOb;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    private void checkForValueValidations(String column, String cellValue, int rowNum, String transportMode) throws ValidationException {
        if (column.equalsIgnoreCase("grossWeight") && cellValue.contains("-")) {
            throw new ValidationException("Gross Weight cannot be negative at row: " + rowNum);
        }
        if (column.equalsIgnoreCase("netWeight") && cellValue.contains("-")) {
            throw new ValidationException("Net Weight cannot be negative at row: " + rowNum);
        }
        if (column.equalsIgnoreCase("tareWeight") && cellValue.contains("-")) {
            throw new ValidationException("Tare Weight cannot be negative at row: " + rowNum);
        }
        if (column.equalsIgnoreCase("measurement") && cellValue.contains("-")) {
            throw new ValidationException("Measurement cannot be negative at row: " + rowNum);
        }
        if (column.equalsIgnoreCase("packs")) {
            try {
                if (transportMode != null &&
                        transportMode.equals(Constants.TRANSPORT_MODE_AIR) &&
                        (cellValue == null || Integer.parseInt(cellValue) < 1)) {
                    throw new ValidationException("Packs is not valid at row: " + rowNum);
                }
                int t = Integer.parseInt(cellValue);
                if (t < 1)
                    throw new ValidationException("Packs is not valid at row: " + rowNum);
            } catch (Exception e) {
                throw new ValidationException("Packs is not valid at row: " + rowNum + ". Please provide integer value and within the range of integer.");
            }
        }
    }

    private void checkForUnitValidations(Map<String, Set<String>> masterListsMap, String column, String cellValue, int rowNum, String transportMode)
            throws ValidationException {
        //MasterData validations : weight unit , volume unit
        if (column.toLowerCase().contains("weightunit")) {
            switch (column.toLowerCase()) {
                case "netweightunit": {
                    if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.WEIGHT_UNIT) &&
                            !masterListsMap.get(MasterDataType.WEIGHT_UNIT.getDescription()).contains(cellValue)) {
                        throw new ValidationException("Net weight unit is invalid at row: " + rowNum);
                    }
                    break;
                }
                case "grossweightunit": {
                    if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.WEIGHT_UNIT) &&
                            !masterListsMap.get(MasterDataType.WEIGHT_UNIT).contains(cellValue)) {
                        throw new ValidationException("Gross weight unit is null or invalid at row: " + rowNum);
                    }
                    break;
                }
                case "tareweightunit": {
                    if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.WEIGHT_UNIT) &&
                            !masterListsMap.get(MasterDataType.WEIGHT_UNIT).contains(cellValue)) {
                        throw new ValidationException("Tare weight unit is null or invalid at row: " + rowNum);
                    }
                    break;
                }
            }
        }
        if (column.toLowerCase().contains("tempunit")) {
            if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.TEMPERATURE_UNIT) &&
                    !masterListsMap.get(MasterDataType.TEMPERATURE_UNIT).contains(cellValue)) {
                throw new ValidationException("Temp Unit is invalid at row: " + rowNum);
            }
        }
        if (column.toLowerCase().contains("packstype")) {
            if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.PACKS_UNIT)
                    && !masterListsMap.get(MasterDataType.PACKS_UNIT).contains(cellValue)) {
                throw new ValidationException("Packs Type is invalid at row: " + rowNum);
            }
        }
        if (column.equalsIgnoreCase("innerPackageType")) {
            if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.PACKS_UNIT) && !masterListsMap.get(MasterDataType.PACKS_UNIT).contains(cellValue)) {
                throw new ValidationException("Inner package type is invalid at row: " + rowNum);
            }
        }
        if (column.toLowerCase().contains("measurementunit")) {
            if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.DIMENSION_UNIT) &&
                    !masterListsMap.get(MasterDataType.DIMENSION_UNIT).contains(cellValue)) {
                throw new ValidationException("Measurement unit is invalid at row: " + rowNum);
            }
        }
        if (column.toLowerCase().contains("volumeunit")) {
            switch (column.toLowerCase()) {
                case "grossvolumeunit": {
                    if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.VOLUME_UNIT) &&
                            !masterListsMap.get(MasterDataType.VOLUME_UNIT).contains(cellValue)) {
                        throw new ValidationException("Gross Volume unit is null or invalid at row: " + rowNum);
                    }
                }
                case "allocatedvolumeunit": {
                    if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.VOLUME_UNIT) &&
                            !masterListsMap.get(MasterDataType.VOLUME_UNIT).contains(cellValue)) {
                        throw new ValidationException("Allocated Volume unit is null or invalid at row: " + rowNum);
                    }
                    break;
                }
                case "achievedvolumeunit": {
                    if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.VOLUME_UNIT) &&
                            !masterListsMap.get(MasterDataType.VOLUME_UNIT).contains(cellValue)) {
                        throw new ValidationException("Achieved Volume unit is null or invalid at row: " + rowNum);
                    }
                    break;
                }
            }
        }
        if (column.toLowerCase().contains("containermode")) {
            if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.CONTAINER_MODE) &&
                    !masterListsMap.get(MasterDataType.CONTAINER_MODE).contains(cellValue)) {
                throw new ValidationException("Container Mode is invalid at row: " + rowNum);
            }
        }
        if (column.toLowerCase().contains("containercode")) {
            if (cellValue.isEmpty() && transportMode.equals(Constants.TRANSPORT_MODE_AIR)) {
                throw new ValidationException("Container Type Code cannot be null at row " + rowNum);
            }
        }
        if (column.toLowerCase().contains("innerpackagemeasurementunit")) {
            if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.DIMENSION_UNIT) &&
                    !masterListsMap.get(MasterDataType.DIMENSION_UNIT).contains(cellValue)) {
                throw new ValidationException("Inner package meaurement unit is invalid at row: " + rowNum);
            }
        }
        if (column.toLowerCase().contains("chargeableunit")) {
            if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.WEIGHT_UNIT) && !masterListsMap.get(MasterDataType.WEIGHT_UNIT).contains(cellValue)) {
                throw new ValidationException("Chargeable unit is invalid at row: " + rowNum);
            }
        }
        if (column.toLowerCase().contains("containerCode")) {
            if (!cellValue.isEmpty() && !masterListsMap.containsKey("ContainerTypes") && !masterListsMap.get("ContainerTypes").contains(cellValue)) {
                throw new ValidationException("Container Type " + cellValue + "is not valid at row " + rowNum);
            }
        }
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

    private Map<String, Set<String>> getAllMasterData(List<String> unlocationsList, List<String> commodityCodesList) {
        Map<String, Set<String>> masterDataMap = new HashMap<>();
        var weightUnitMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.WEIGHT_UNIT, masterDataMap)), executorService);
        var volumeUnitMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.VOLUME_UNIT, masterDataMap)), executorService);
        var temperatureUnitMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.TEMPERATURE_UNIT, masterDataMap)), executorService);
        var hblModeMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.HBL_DELIVERY_MODE, masterDataMap)), executorService);
        var dimensionUnitMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.DIMENSION_UNIT, masterDataMap)), executorService);
        var dgClassMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.DG_CLASS, masterDataMap)), executorService);
        var packUnitMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.PACKS_UNIT, masterDataMap)), executorService);
        var containerTypeMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchContainerType(masterDataMap)), executorService);
        var unlocationMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchUnlocationData(unlocationsList, masterDataMap)), executorService);
        var commodityMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchCommodityData(commodityCodesList, masterDataMap)), executorService);

        CompletableFuture.allOf(weightUnitMasterData, volumeUnitMasterData, temperatureUnitMasterData, hblModeMasterData, dimensionUnitMasterData, dgClassMasterData, packUnitMasterData, containerTypeMasterData, unlocationMasterData, commodityMasterData).join();

        return masterDataMap;
    }

    public void fetchMasterLists(MasterDataType masterDataType, Map<String, Set<String>> masterDataMap) {
        MasterListRequest masterListRequest = MasterListRequest.builder().ItemType(masterDataType.getDescription()).build();
        MasterListRequestV2 masterListRequests = new MasterListRequestV2();
        masterListRequests.getMasterListRequests().add(masterListRequest);
        V1DataResponse v1DataResponse = v1Service.fetchMultipleMasterData(masterListRequests);
        if (v1DataResponse != null) {
            if (v1DataResponse.entities instanceof List<?>) {
                List<MasterData> masterDataList = jsonHelper.convertValueToList(v1DataResponse.entities, MasterData.class);
                if (masterDataList != null && !masterDataList.isEmpty()) {
                    Set<String> masterDataSet = masterDataList.stream().filter(Objects::nonNull).map(MasterData::getItemValue).collect(Collectors.toSet());
                    masterDataMap.put(masterDataType.getDescription(), masterDataSet);
                }
            }
        }
    }

    public void fetchContainerType(Map<String, Set<String>> masterDataMap) {
        V1DataResponse v1DataResponse = v1Service.fetchContainerTypeData(null);
        if (v1DataResponse != null) {
            if (v1DataResponse.entities instanceof List<?>) {
                List<V1ContainerTypeResponse> containerTypeList = jsonHelper.convertValueToList(v1DataResponse.entities, V1ContainerTypeResponse.class);
                if (containerTypeList != null && !containerTypeList.isEmpty()) {
                    Set<String> containerTypeSet = containerTypeList.stream().filter(Objects::nonNull).map(V1ContainerTypeResponse::getCode).collect(Collectors.toSet());
                    masterDataMap.put("ContainerTypes", containerTypeSet);
                }
            }
        }
    }

    public void fetchUnlocationData(List<String> unlocationsList, Map<String, Set<String>> masterDataMap) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of("LocCode"));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(unlocationsList)));
        request.setCriteriaRequests(criteria);
        V1DataResponse v1DataResponse = v1Service.fetchUnlocation(request);
        if (v1DataResponse != null) {
            if (v1DataResponse.entities instanceof List<?>) {
                List<UnlocationsResponse> unlocationList = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
                if (unlocationList != null && !unlocationList.isEmpty()) {
                    Set<String> unlocationSet = unlocationList.stream().filter(Objects::nonNull).map(UnlocationsResponse::getLocCode).collect(Collectors.toSet());
                    masterDataMap.put("Unlocations", unlocationSet);
                }
            }
        }
    }

    public void fetchCommodityData(List<String> commodityCodesList, Map<String, Set<String>> masterDataMap) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>();
        List<Object> field = new ArrayList<>(List.of("Code"));
        String operator = Operators.IN.getValue();
        criteria.addAll(List.of(field, operator, List.of(commodityCodesList)));
        request.setCriteriaRequests(criteria);
        V1DataResponse response = v1Service.fetchCommodityData(request);
        if (response != null) {
            if (response.entities instanceof List<?>) {
                List<CommodityResponse> commodityList = jsonHelper.convertValueToList(response.entities, CommodityResponse.class);
                if (commodityList != null && !commodityList.isEmpty()) {
                    Set<String> commoditySet = commodityList.stream().filter(Objects::nonNull).map(CommodityResponse::getCode).collect(Collectors.toSet());
                    masterDataMap.put("CommodityCodes", commoditySet);
                }
            }
        }
    }

    public Runnable withMdc(Runnable runnable) {
        Map<String, String> mdc = MDC.getCopyOfContextMap();
        String token = RequestAuthContext.getAuthToken();
        return () -> {
            MDC.setContextMap(mdc);
            RequestAuthContext.setAuthToken(token);
            runnable.run();
        };
    }

}
