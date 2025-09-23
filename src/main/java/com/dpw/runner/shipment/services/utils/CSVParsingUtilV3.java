package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentVersionContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ContainerConstants;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.dao.impl.ConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1ContainerTypeResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferDGSubstance;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.CommodityResponse;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import com.fasterxml.jackson.core.type.TypeReference;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.text.WordUtils;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Slf4j
@Component
public class CSVParsingUtilV3<T> {

    private ConsoleShipmentMappingDao consoleShipmentMappingDao;

    private IShipmentDao shipmentDao;

    private IV1Service v1Service;

    private JsonHelper jsonHelper;

    private IConsolidationDetailsDao consolidationDetailsDao;

    ExecutorService executorService;

    private CommonUtils commonUtils;

    @Autowired
    public CSVParsingUtilV3(ConsoleShipmentMappingDao consoleShipmentMappingDao, IShipmentDao shipmentDao, IV1Service v1Service, JsonHelper jsonHelper, IConsolidationDetailsDao consolidationDetailsDao, ExecutorService executorService, CommonUtils commonUtils) {
        this.consoleShipmentMappingDao = consoleShipmentMappingDao;
        this.shipmentDao = shipmentDao;
        this.v1Service = v1Service;
        this.jsonHelper = jsonHelper;
        this.consolidationDetailsDao = consolidationDetailsDao;
        this.executorService = executorService;
        this.commonUtils = commonUtils;
    }

    public String getCamelCase(String name) {
        return WordUtils.uncapitalize(name);
    }

    @SuppressWarnings("java:S1874")
    private T createEntityInstance(Class<T> entityType) throws InstantiationException, IllegalAccessException {
        return entityType.newInstance();
    }

    public List<T> parseExcelFilePacking(MultipartFile file, BulkUploadRequest request, Map<UUID, T> mapOfEntity, Map<String, Set<String>> masterDataMap,
                                         Class<T> entityType, Map<Long, Long> undg, Map<Long, String> flashpoint, Map<String, String> locCodeToLocationReferenceGuidMap, List<String> errorList) throws IOException {

        Set<String> mandatoryColumns = new HashSet<>();
        mandatoryColumns.add("shipmentNumber");
        mandatoryColumns.add(Constants.PACKS);
        List<T> entityList = new ArrayList<>();
        List<String> unlocationsList = new ArrayList<>();
        List<String> commodityCodesList = new ArrayList<>();
        List<Long> dgSubstanceIdList = new ArrayList<>();
        Map<String, Long> dicShipmentId = getShipmentIds(request.getConsolidationId());

        int originPos = -1;
        int commodityCodePos = -1;
        int guidPos = -1;
        int dgSubstanceIdPos = -1;
        int shipmentNumberPos = -1;

        try (Workbook workbook = new XSSFWorkbook(file.getInputStream())) {
            Sheet sheet = workbook.getSheetAt(0); // Assuming data is in the first sheet
            validateExcel(sheet);
            Row headerRow = sheet.getRow(0);
            String[] header = new String[headerRow.getLastCellNum()];
            Set<String> headerSet = new HashSet<>();
            for (int i = 0; i < headerRow.getLastCellNum(); i++) {
                validateExcelColumn(headerRow, i);
                header[i] = getCamelCase(headerRow.getCell(i).getStringCellValue());
                if (mandatoryColumns.contains(header[i])) {
                    mandatoryColumns.remove(header[i]);
                }
                headerSet.add(header[i]);
                if (header[i].equalsIgnoreCase("guid")) {
                    guidPos = i;
                }
                if (header[i].equalsIgnoreCase("DGSubstanceId")) {
                    dgSubstanceIdPos = i;
                }
                if (header[i].equalsIgnoreCase("commodityCode")) {
                    commodityCodePos = i;
                }
                if (header[i].equalsIgnoreCase("origin")) {
                    originPos = i;
                }
                if (header[i].equalsIgnoreCase("shipmentNumber")) {
                    shipmentNumberPos = i;
                }
            }

            validateParseExcelFile(mandatoryColumns, headerSet, headerRow);
            processGuidSetForExcelFile(sheet, commodityCodePos, commodityCodesList, originPos, unlocationsList, guidPos, dgSubstanceIdPos, dgSubstanceIdList, errorList);

            //-----fetching master data in bulk
            Map<String, Set<String>> masterListsMap = getAllMasterDataPacking(unlocationsList, commodityCodesList, masterDataMap, locCodeToLocationReferenceGuidMap);

            setUNDGContactMasterDataAndFlashPointMasterData(dgSubstanceIdList, undg, flashpoint);

            processLastRowNumForExcelFilePacking(request, mapOfEntity, entityType, sheet, guidPos, shipmentNumberPos, dicShipmentId, header, masterListsMap, entityList, errorList);
        } catch (ValidationException e1) {
            log.error(e1.getMessage());
            throw new ValidationException(e1.getMessage());
        } catch (NoSuchFieldException | IllegalAccessException | InstantiationException e) {
            log.debug(ContainerConstants.EXCEL_SHEET_NOT_VALID, e);
            log.error(e.getMessage());
            throw new ValidationException(ContainerConstants.EXCEL_SHEET_INVALID);
        }
        return entityList;
    }

    private void validateParseExcelFile(Set<String> mandatoryColumns, Set<String> headerSet, Row headerRow) {
        if (!mandatoryColumns.isEmpty()) {
            throw new ValidationException(mandatoryColumns.toString() + "column(s) is missing");
        }

        if (headerSet.size() < headerRow.getLastCellNum()) {
            throw new ValidationException(ContainerConstants.INVALID_EXCEL_COLUMNS);
        }
    }

    private void processGuidSetForExcelFile(Sheet sheet, int commodityCodePos, List<String> commodityCodesList, int originPos, List<String> unlocationsList, int guidPos, int dgSubstanceIdPos, List<Long> dgSubstanceIdList, List<String> errorList) {
        Set<String> guidSet = new HashSet<>();
        for (int i = 1; i <= sheet.getLastRowNum(); i++) {
            Row row = sheet.getRow(i);
            if (commodityCodePos != -1) {
                String commodityCode = getCellValueAsString(row.getCell(commodityCodePos));
                if (!StringUtils.isEmpty(commodityCode))
                    commodityCodesList.add(commodityCode);
            }
            if (originPos != -1) {
                String origin = getCellValueAsString(row.getCell(originPos));
                if (!StringUtils.isEmpty(origin))
                    unlocationsList.add(origin);
            }
            if (guidPos != -1) {
                validateDuplicateGuid(guidPos, row, guidSet, i, errorList);
                guidSet.add(getCellValueAsString(row.getCell(guidPos)));
            }
            processDgSubstanceIdPos(dgSubstanceIdPos, dgSubstanceIdList, row, i);

        }
    }

    private void processDgSubstanceIdPos(int dgSubstanceIdPos, List<Long> dgSubstanceIdList, Row row, int i) {
        if (dgSubstanceIdPos != -1) {
            try {
                String dgSubstanceIdCell = getCellValueAsString(row.getCell(dgSubstanceIdPos));
                if (!StringUtils.isEmpty(dgSubstanceIdCell)) {
                    Long dgSubstanceIdVal = Long.parseLong(dgSubstanceIdCell);
                    dgSubstanceIdList.add(dgSubstanceIdVal);
                }
            } catch (Exception ex) {
                throw new ValidationException("DGSubstanceId is invalid at row: " + i);
            }
        }
    }

    @SuppressWarnings("java:S1130") // Suppressing NoSuchFieldException sonar issue
    private void processLastRowNumForExcelFilePacking(BulkUploadRequest request, Map<UUID, T> mapOfEntity, Class<T> entityType, Sheet sheet, int guidPos, int shipmentNumberPos, Map<String, Long> dicShipmentId, String[] header, Map<String, Set<String>> masterListsMap, List<T> entityList, List<String> errorList) throws InstantiationException, IllegalAccessException, NoSuchFieldException {
        for (int i = 1; i <= sheet.getLastRowNum(); i++) {
            Row row = sheet.getRow(i);
            validateGuidPos(mapOfEntity, guidPos, row, i, errorList);
            T entity = guidPos != -1 && mapOfEntity != null ? mapOfEntity.get(UUID.fromString(getCellValueAsString(row.getCell(guidPos)))) : createEntityInstance(entityType);

            validateShipmentNumberPos(shipmentNumberPos, dicShipmentId, row, (Packing) entity, i);
            for (int j = 0; j < header.length; j++) {
                Cell cell = row.getCell(j);
                if (cell != null) {
                    String cellValue = getCellValueAsString(cell);
                    checkForUnitValidations(masterListsMap, header[j], cellValue, i, request.getTransportMode(), errorList);
                    checkForValueValidations(header[j], masterListsMap, cellValue, i, request.getTransportMode(), errorList);
                    validateOrigin(header, masterListsMap, j, cellValue, i);
                    setField(entity, header[j], cellValue, i);
                }
            }

            entityList.add(entity);
        }
    }

    private void validateOrigin(String[] header, Map<String, Set<String>> masterListsMap, int j, String cellValue, int i) {
        if (header[j].equalsIgnoreCase("origin") && masterListsMap.containsKey(Constants.UNLOCATIONS)
                && !masterListsMap.get(Constants.UNLOCATIONS).contains(cellValue)) {
            throw new ValidationException("Origin " + cellValue + "is not valid at row " + i);
        }
    }

    private void validateGuidPos(Map<UUID, T> mapOfEntity, int guidPos, Row row, int i, List<String> errorList) {
        if (guidPos != -1) {
            String guidVal = getCellValueAsString(row.getCell(guidPos));
            try {
                if (!StringUtils.isEmpty(guidVal)) {
                    var packingGuid = UUID.fromString(guidVal);
                    if (mapOfEntity != null && !mapOfEntity.containsKey(packingGuid)) {
                        errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, i, "Guid", guidVal));
                    }
                }
            } catch (Exception ex) {
                errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, i, "Guid", guidVal));
            }
        }
    }

    private void validateShipmentNumberPos(int shipmentNumberPos, Map<String, Long> dicShipmentId, Row row, Packing entity, int i) {
        if (shipmentNumberPos != -1) {
            var shipmentNumber = getCellValueAsString(row.getCell(shipmentNumberPos));
            if (!StringUtils.isEmpty(shipmentNumber)) {
                if (dicShipmentId.containsKey(shipmentNumber)) {
                    entity.setShipmentId(dicShipmentId.get(shipmentNumber));
                    entity.setShipmentNumber(shipmentNumber);
                } else {
                    throw new ValidationException("Shipment Number " + shipmentNumber + " is not valid at row: " + i);
                }
            } else {
                throw new ValidationException("Shipment Number is null or empty at row: " + i);
            }
        }
    }

    private Map<String, Long> getShipmentIds(Long consolidationId) {
        var consoleShipmentMap = consoleShipmentMappingDao.findByConsolidationId(consolidationId);
        List<Long> lstShipmentId = new ArrayList<>();
        for (var temp : consoleShipmentMap) {
            lstShipmentId.add(temp.getShipmentId());
        }
        var lstRequest = constructListCommonRequest("id", lstShipmentId, "IN");
        Pair<Specification<ShipmentDetails>, Pageable> pair2 = fetchData(lstRequest, ShipmentDetails.class);
        Page<ShipmentDetails> shipmentPage = shipmentDao.findAll(pair2.getLeft(), pair2.getRight());
        List<ShipmentDetails> lstShipments = shipmentPage.getContent();
        Map<String, Long> dictionary = new HashMap<>();
        for (var shipment : lstShipments) {
            dictionary.put(shipment.getShipmentId(), shipment.getId());
        }
        return dictionary;
    }

    public List<T> parseExcelFile(MultipartFile file, BulkUploadRequest request, Map<UUID, T> mapOfEntity,
                                  Map<String, Set<String>> masterDataMap, Class<T> entityType, Class<?> modelClass,
                                  Map<Long, Long> undg, Map<Long, String> flashpoint,
                                  Map<String, String> locCodeToLocationReferenceGuidMap, List<String> errorList, List<String> excelHeaders) throws IOException {
        if (entityType.equals(Packing.class)) {
            return parseExcelFilePacking(file, request, mapOfEntity, masterDataMap, entityType, undg, flashpoint, locCodeToLocationReferenceGuidMap, errorList);
        }
        if (entityType.equals(Events.class)) {
            return parseExcelFileEvents(file, request, masterDataMap, entityType);
        }
        return parseExcelGeneric(file, request, mapOfEntity, masterDataMap, entityType, modelClass, locCodeToLocationReferenceGuidMap, errorList, excelHeaders);
    }

    private List<T> parseExcelGeneric(MultipartFile file, BulkUploadRequest request, Map<UUID, T> mapOfEntity,
                                      Map<String, Set<String>> masterDataMap, Class<T> entityType, Class<?> modelClass,
                                      Map<String, String> locCodeToLocationReferenceGuidMap, List<String> errorList, List<String> excelHeaders) throws IOException {
        List<T> entityList = new ArrayList<>();
        List<String> unlocationsList = new ArrayList<>();
        List<String> commodityCodesList = new ArrayList<>();
        int containerStuffingLocationPos = -1;
        int commodityCodePos = -1;
        int guidPos = -1;

        try (Workbook workbook = new XSSFWorkbook(file.getInputStream())) {
            Sheet sheet = workbook.getSheetAt(0);
            validateExcel(sheet);
            Row headerRow = sheet.getRow(0);
            String[] header = extractHeader(headerRow, modelClass, excelHeaders);
            guidPos = findColumnIndex(header, "guid");
            containerStuffingLocationPos = findColumnIndex(header, "containerStuffingLocation");
            commodityCodePos = findColumnIndex(header, "commodityCode");
            addGuidInList(sheet, commodityCodePos, commodityCodesList, containerStuffingLocationPos, unlocationsList, guidPos, errorList);
            Map<String, Set<String>> masterListsMap =
                    getAllMasterDataContainer(unlocationsList, commodityCodesList, masterDataMap, locCodeToLocationReferenceGuidMap);
            Map<String, String> existingContainerNumbers = new HashMap<>();
            processSheetLastRowNum(request, mapOfEntity, entityType, sheet, guidPos, header, masterListsMap, existingContainerNumbers, entityList, errorList);

        } catch (ValidationException e1) {
            log.error(e1.getMessage());
            throw new ValidationException(e1.getMessage());
        } catch (NoSuchFieldException | IllegalAccessException | InstantiationException e) {
            log.debug(ContainerConstants.EXCEL_SHEET_NOT_VALID, e);
            log.error(e.getMessage());
            throw new ValidationException(ContainerConstants.EXCEL_SHEET_INVALID);
        }
        return entityList;
    }

    private String[] extractHeader(Row headerRow, Class<?> modelClass, List<String> excelHeaders) {
        Field[] fields = modelClass.getDeclaredFields();
        Map<String, String> renameFieldMap = Arrays.stream(fields)
                .filter(x -> x.isAnnotationPresent(ExcelCell.class))
                .collect(Collectors.toMap(
                        x -> ShipmentVersionContext.isV3() && !x.getAnnotation(ExcelCell.class).displayNameOverride().isEmpty()
                                ? x.getAnnotation(ExcelCell.class).displayNameOverride()
                                : x.getAnnotation(ExcelCell.class).displayName(),
                        Field::getName));
        List<String> mappedHeaders = new ArrayList<>();
        List<String> headers = new ArrayList<>();
        int lastUsedIndex = -1;
        for (int i = 0; i < headerRow.getLastCellNum(); i++) {
            Cell cell = headerRow.getCell(i, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL);
            String value = getCellValueAsString(cell);
            if (value != null && !value.trim().isEmpty()) {
                lastUsedIndex = i; // keep updating until the rightmost filled cell
            }
        }

        if (lastUsedIndex == -1) {
            throw new ValidationException("No column headers found. Please upload a valid file.");
        }

        // iterate only until the last used index
        for (int i = 0; i <= lastUsedIndex; i++) {
            Cell cell = headerRow.getCell(i, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL);
            String value = getCellValueAsString(cell);

            if (value == null || value.trim().isEmpty()) {
                // Blank before last non-empty cell → error
                throw new ValidationException(
                        "Blank column header found at position " + (i + 1)
                                + ". Please remove empty columns in between headers.");
            }

            headers.add(value.trim());
            mappedHeaders.add(renameFieldMap.getOrDefault(value.trim(),
                    getCamelCase(value.trim())));
        }
        excelHeaders.addAll(headers);
        Set<String> headerSet = new HashSet<>(mappedHeaders);
        validateHeaderUniqueness(headerSet, lastUsedIndex);
        return mappedHeaders.toArray(String[]::new);
    }


    public void validateHeaders(List<String> headers, List<String> errorList) {
        TypeReference<List<String>> typeRef = new TypeReference<>() {};
        List<String> mandatoryFields = commonUtils.getAppConfigValueByKey("MANDATORY_FIELD_FOR_CONTAINER_UPLOAD", typeRef);
        Set<String> headerSet = new HashSet<>(headers);
        errorList.addAll(mandatoryFields.stream().filter(field -> !headerSet.contains(field)).map(field -> "Row# 1 : " + field + " is mandatory").toList());
    }

    private int findColumnIndex(String[] header, String columnName) {
        for (int i = 0; i < header.length; i++) {
            if (header[i].equalsIgnoreCase(columnName)) {
                return i;
            }
        }
        return -1;
    }

    private void validateHeaderUniqueness(Set<String> headerSet, int totalColumns) {
        if (headerSet.size() < totalColumns) {
            throw new ValidationException(ContainerConstants.INVALID_EXCEL_COLUMNS);
        }
    }


    private void validateExcelColumn(Row headerRow, int i) {
        if (headerRow.getCell(i) == null || StringUtility.isEmpty(headerRow.getCell(i).getStringCellValue())) {
            throw new ValidationException(ContainerConstants.INVALID_EXCEL_COLUMNS);
        }
    }

    private void addGuidInList(Sheet sheet, int commodityCodePos, List<String> commodityCodesList, int containerStuffingLocationPos, List<String> unlocationsList, int guidPos, List<String> errorList) {
        Set<String> guidSet = new HashSet<>();
        for (int i = 1; i <= sheet.getLastRowNum(); i++) {
            Row row = sheet.getRow(i);
            if (commodityCodePos != -1) {
                if (Objects.isNull(row.getCell(commodityCodePos))) {
                    throw new ValidationException("Please enter at least One container line in the upload file.");
                }
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
                validateDuplicateGuid(guidPos, row, guidSet, i, errorList);
                guidSet.add(getCellValueAsString(row.getCell(guidPos)));
            }
        }
    }

    private void validateDuplicateGuid(int guidPos, Row row, Set<String> guidSet, int i, List<String> errorList) {
        String guidCell = getCellValueAsString(row.getCell(guidPos));
        if (!StringUtils.isEmpty(guidCell) && guidSet.contains(guidCell)) {
            errorList.add(String.format(ContainerConstants.GENERIC_DUPLICATE_FIELD_MSG, i+1, "Guid", guidCell));
        }
    }

    @SuppressWarnings("java:S1130") // Suppressing NoSuchFieldException sonar issue
    private void processSheetLastRowNum(BulkUploadRequest request, Map<UUID, T> mapOfEntity, Class<T> entityType, Sheet sheet, int guidPos, String[] header, Map<String, Set<String>> masterListsMap, Map<String, String> existingContainerNumbers, List<T> entityList, List<String> errorList) throws InstantiationException, IllegalAccessException, NoSuchFieldException {
        for (int i = 1; i <= sheet.getLastRowNum(); i++) {
            Row row = sheet.getRow(i);
            int rowNum = i+1;
            boolean isUpdate = false;
            if (guidPos != -1) { // means that guid column is present.
                String guidVal = getCellValueAsString(row.getCell(guidPos));
                if (validateSheetLastRowNum(mapOfEntity, guidVal, rowNum, errorList)){
                    continue;
                }
            }
            T entity = guidPos != -1 && mapOfEntity != null && row.getCell(guidPos) != null && getCellValueAsString(row.getCell(guidPos)) != null && !getCellValueAsString(row.getCell(guidPos)).trim().isEmpty() ? mapOfEntity.get(UUID.fromString(getCellValueAsString(row.getCell(guidPos)))) : createEntityInstance(entityType);
            if (mapOfEntity != null && guidPos != -1 && row.getCell(guidPos) != null && getCellValueAsString(row.getCell(guidPos)) != null && !getCellValueAsString(row.getCell(guidPos)).trim().isEmpty() && mapOfEntity.containsKey(UUID.fromString(getCellValueAsString(row.getCell(guidPos))))) {
                isUpdate = true;
            }
            processHeaderForExcel(request, header, row, masterListsMap, rowNum, guidPos, isUpdate, existingContainerNumbers, entity, errorList);

            entityList.add(entity);
        }
    }

    private boolean validateSheetLastRowNum(Map<UUID, T> mapOfEntity, String guidVal, int rowNum, List<String> errorList) {
        try {
            if (!StringUtils.isEmpty(guidVal)) {
                var containerGuid = UUID.fromString(guidVal);
                if (mapOfEntity != null && !mapOfEntity.containsKey(containerGuid)) {
                    throw new ValidationException(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Guid", guidVal));
                }
                return false;
            }
        } catch (ValidationException ex) {
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum,  "Guid", guidVal));
            return true;
        }
        return false;
    }

    private void processHeaderForExcel(BulkUploadRequest request, String[] header, Row row, Map<String, Set<String>> masterListsMap, int rowNum, int guidPos, boolean isUpdate, Map<String, String> existingContainerNumbers, T entity, List<String> errorList) {
        String prevCellValue = "";
        boolean isDgContainer;
        for (int j = 0; j < header.length; j++) {//
            Cell cell = row.getCell(j);
            String cellValue = getCellValueAsString(cell);
            isDgContainer = parseBooleanField(header[j], "hazardous", "Dangerous Goods", cellValue, rowNum, errorList);
            checkForUnitValidations(masterListsMap, header[j], cellValue, rowNum, request.getTransportMode(), errorList);
            if (header[j].equalsIgnoreCase("containerCode"))
                checkForContainerCodeValidation(masterListsMap, cellValue, rowNum, errorList);
            checkForDependentFields(header, prevCellValue, cellValue, rowNum, j, errorList, masterListsMap, isDgContainer);
            checkForValueValidations(header[j], masterListsMap, cellValue, rowNum, request.getTransportMode(), errorList);//
            if (header[j].equalsIgnoreCase(Constants.CONTAINER_NUMBER))
                checkForDuplicateContainerNumberValidation(guidPos, row, cellValue, rowNum, isUpdate, existingContainerNumbers);
            setField(entity, header[j], cellValue, rowNum);
            prevCellValue = cellValue;
        }
    }

    private boolean parseBooleanField(String header, String headerName, String msgField, String cellValue, int rowNum, List<String> errorList) {
        if (header.equalsIgnoreCase(headerName)) {
            if (cellValue == null || cellValue.trim().isEmpty()) {
                errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, msgField, cellValue));
                return false;
            }

            String normalized = cellValue.trim().toLowerCase();
            return switch (normalized) {
                case "yes", "y", "true" -> true;
                case "no", "n", "false" -> false;
                default -> {
                    errorList.add(String.format(ContainerConstants.GENERIC_INVALID_BOOLEAN_FIELD_MSG, rowNum, msgField, cellValue));
                    yield false;
                }
            };
        }
        return false;
    }



    private void checkForDependentFields(String[] header, String prevCellValue, String cellValue, int rowNum, int j, List<String> errorList, Map<String, Set<String>> masterListsMap, Boolean isDgContainer) {
        String currentColumn = StringUtility.convertToString(header[j]);
        String prevColumn = "";
        if (j>0) {
            prevColumn = StringUtility.convertToString(header[j - 1]);
        }
        if (Boolean.TRUE.equals(isDgContainer)) {
            masterdataValidation(cellValue, currentColumn, "dgClass", masterListsMap, "DGClass", rowNum, "DG Class", errorList);
            validateMandatoryField(currentColumn, "dgClass", rowNum, cellValue, "DG Class", errorList);
            validateMandatoryField(currentColumn, Constants.GROSS_WEIGHT, rowNum, cellValue, "Cargo Wt.", errorList);
            if (currentColumn.equalsIgnoreCase("unNumber") && cellValue != null && !cellValue.trim().matches("\\d+")) {
                errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "UN Number", cellValue));
            }
            validateMandatoryField(currentColumn, "unNumber", rowNum, cellValue, "UN Number", errorList);
            fieldLengthValidation(currentColumn, "unNumber", cellValue, 31, rowNum, "UN Number", errorList);
            validateMandatoryField(currentColumn, "ProperShippingName", rowNum, cellValue, "Proper Shipping Name", errorList);
            masterdataValidation(cellValue, currentColumn, "packingGroup", masterListsMap, "PackingGroup", rowNum, "Packing Group", errorList);
            validateDependentMandatoryFields(currentColumn, "minimumFlashPointUnit", cellValue, prevColumn, "minimumFlashPoint", prevCellValue, rowNum, "Min. Flash Point Unit", errorList);
            masterdataValidation(cellValue, currentColumn, "minimumFlashPointUnit", masterListsMap, "TemperatureUnit", rowNum, "Min. Flash Point Unit", errorList);
        }
        masterdataValidation(cellValue, currentColumn, "hblDeliveryMode", masterListsMap, "HBLDeliveryMode", rowNum, "Mode", errorList);
        validateDependentMandatoryFields(currentColumn, "minTempUnit", cellValue, prevColumn, "minTemp", prevCellValue, rowNum, "Min Temp Unit", errorList);
        masterdataValidation(cellValue, currentColumn, "minTempUnit", masterListsMap, "TemperatureUnit", rowNum, "Min Temp Unit", errorList);
        parseBooleanField(header[j], "isEmpty", "Empty Container", cellValue, rowNum, errorList);
        parseBooleanField(header[j], "isShipperOwned", "Shipper Owned Container", cellValue, rowNum, errorList);
        parseBooleanField(header[j], "isOwnContainer", "DP World Container", cellValue, rowNum, errorList);
        parseBooleanField(header[j], "isReefer", "Is Reefer", cellValue, rowNum, errorList);
        validateDependentMandatoryFields(currentColumn, "packsType", cellValue, prevColumn, "packs", prevCellValue, rowNum, "Packages Unit", errorList);
        masterdataValidation(cellValue, currentColumn, "packsType", masterListsMap, MasterDataType.PACKS_UNIT.getDescription(), rowNum, "Packages Unit", errorList);
        validateDependentMandatoryFields(currentColumn, "grossWeightUnit", cellValue, prevColumn, "grossWeight", prevCellValue, rowNum, "Cargo Wt. Unit", errorList);
        masterdataValidation(cellValue, currentColumn, "grossWeightUnit", masterListsMap, MasterDataType.WEIGHT_UNIT.getDescription(), rowNum, "Cargo Wt. Unit", errorList);
        validateDependentMandatoryFields(currentColumn, "grossVolumeUnit", cellValue, prevColumn, "grossVolume", prevCellValue, rowNum, "Vol. Unit", errorList);
        masterdataValidation(cellValue, currentColumn, "grossVolumeUnit", masterListsMap, MasterDataType.VOLUME_UNIT.getDescription(), rowNum, "Vol. Unit", errorList);
        validateDependentMandatoryFields(currentColumn, "tareWeightUnit", cellValue, prevColumn, "tareWeight", prevCellValue, rowNum, "Tare Wt. Unit", errorList);
        masterdataValidation(cellValue, currentColumn, "tareWeightUnit", masterListsMap, MasterDataType.WEIGHT_UNIT.getDescription(), rowNum, "Tare Wt. Unit", errorList);
        validateDependentMandatoryFields(currentColumn, "netWeightUnit", cellValue, prevColumn, "netWeight", prevCellValue, rowNum, "Gr. Wt. Unit", errorList);
        masterdataValidation(cellValue, currentColumn, "netWeightUnit", masterListsMap, MasterDataType.WEIGHT_UNIT.getDescription(), rowNum, "Gr. Wt. Unit", errorList);
    }

    private void masterdataValidation(String cellValue, String currentColumn, String actualColumn, Map<String, Set<String>> masterListsMap, String masterKey, int rowNum, String validationMsg, List<String> errorList) {
        if (StringUtils.isNotEmpty(cellValue) && currentColumn.equalsIgnoreCase(actualColumn) && masterListsMap.containsKey(masterKey)
                && !masterListsMap.get(masterKey).contains(cellValue)) {
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, validationMsg, cellValue));
        }
    }

    private void fieldLengthValidation(String currentColumn, String actualColumn, String cellValue, int allowedLength, int rowNum, String lengthValidationMsg, List<String> errorList) {
        if (currentColumn.equalsIgnoreCase(actualColumn) && cellValue != null && cellValue.length() > allowedLength) {
            errorList.add(String.format(ContainerConstants.GENERIC_LENGTH_VALIDATION_MSG, rowNum, lengthValidationMsg, StringUtility.convertToString(allowedLength)));
        }
    }

    private void validateDependentMandatoryFields(String currentColumn, String actualColumn, String cellValue, String prevColumn, String actualPreviousColumn, String prevCellValue, int rowNum, String mandatoryColumn, List<String> errorList) {
        if (currentColumn.equalsIgnoreCase(actualColumn) && StringUtils.isEmpty(cellValue) && prevColumn.equalsIgnoreCase(actualPreviousColumn) && StringUtils.isNotEmpty(prevCellValue)) {
            errorList.add(String.format(ContainerConstants.GENERIC_MANDATORY_FIELD_MSG, rowNum, mandatoryColumn));
        }
    }

    private void validateMandatoryField(String currentColumn, String actualColumn, int rowNum, String cellValue, String mandatoryColumn, List<String> errorList) {
        if (currentColumn.equalsIgnoreCase(actualColumn) && StringUtils.isEmpty(cellValue)) {
            errorList.add(String.format(ContainerConstants.GENERIC_MANDATORY_FIELD_MSG, rowNum, mandatoryColumn));
        }
    }


    public List<T> parseExcelFileEvents(MultipartFile file, BulkUploadRequest request,
                                        Map<String, Set<String>> masterDataMap, Class<T> entityType) throws IOException {
        if (request.getConsolidationId() == null) {
            throw new ValidationException("Please save the consolidation and then try again.");
        }
        var consolOpt = consolidationDetailsDao.findById(request.getConsolidationId());
        if (consolOpt.isEmpty()) {
            throw new ValidationException("Consolidation id is invalid " + request.getConsolidationId());
        }
        var consol = consolOpt.get();

        List<T> entityList = new ArrayList<>();
        List<String> containerNumberList = new ArrayList<>();
        Set<String> mandatoryColumns = new HashSet<>();
        mandatoryColumns.add("eventCode");
        mandatoryColumns.add("containerNumber");
        mandatoryColumns.add(Constants.CONTAINER_NUMBER);

        try (Workbook workbook = new XSSFWorkbook(file.getInputStream())) {
            Sheet sheet = workbook.getSheetAt(0); // Assuming data is in the first sheet
            validateExcel(sheet);
            Row headerRow = sheet.getRow(0);
            String[] header = new String[headerRow.getLastCellNum()];
            Set<String> headerSet = new HashSet<>();
            for (int i = 0; i < headerRow.getLastCellNum(); i++) {
                validateExcelColumn(headerRow, i);
                header[i] = getCamelCase(headerRow.getCell(i).getStringCellValue());
                headerSet.add(header[i]);

                if (mandatoryColumns.contains(header[i])) {
                    mandatoryColumns.remove(header[i]);
                }
            }

            if (!mandatoryColumns.isEmpty()) {
                throw new ValidationException(mandatoryColumns.toString() + "column(s) is missing");
            }


            Map<String, Set<String>> masterListsMap = getAllMasterDataEvents(masterDataMap);

            if (headerSet.size() < headerRow.getLastCellNum()) {
                throw new ValidationException(ContainerConstants.INVALID_EXCEL_COLUMNS);
            }

            Set<String> orderEventsDictionary = masterListsMap.get(MasterDataType.ORDER_EVENTS.getDescription());
            Set<String> existingContainerNumberSet = consol.getContainersList()
                    .stream().map(Containers::getContainerNumber)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toSet());
            for (int i = 1; i <= sheet.getLastRowNum(); i++) {
                Row row = sheet.getRow(i);
                T entity = createEntityInstance(entityType);
                processHeader(header, row, i, existingContainerNumberSet, containerNumberList, orderEventsDictionary, entity);

                entityList.add(entity);
            }
        } catch (ValidationException e1) {
            log.error(e1.getMessage());
            throw new ValidationException(e1.getMessage());
        } catch (NoSuchFieldException | IllegalAccessException | InstantiationException e) {
            log.debug(ContainerConstants.EXCEL_SHEET_NOT_VALID, e);
            log.error(e.getMessage());
            throw new ValidationException(ContainerConstants.EXCEL_SHEET_INVALID);
        }
        return entityList;
    }

    private void processHeader(String[] header, Row row, int i, Set<String> existingContainerNumberSet, List<String> containerNumberList, Set<String> orderEventsDictionary, T entity) throws NoSuchFieldException, IllegalAccessException {
        for (int j = 0; j < header.length; j++) {
            Cell cell = row.getCell(j);
            if (cell != null) {
                String cellValue = getCellValueAsString(cell);
                validateContainerNumber(header, i, existingContainerNumberSet, containerNumberList, j, cellValue);
                if (header[j].equalsIgnoreCase("eventCode")) {
                    if (StringUtils.isEmpty(cellValue)) {
                        throw new ValidationException("EventCode is mandatory at row: " + i);
                    }
                    if (orderEventsDictionary != null && !orderEventsDictionary.contains(cellValue)) {
                        throw new ValidationException("EventCode is not present in masterData at row: " + i);
                    }
                }
                setFieldForEvents(entity, header[j], cellValue);
            }
        }
    }

    private void validateContainerNumber(String[] header, int i, Set<String> existingContainerNumberSet, List<String> containerNumberList, int j, String cellValue) {
        if (header[j].equalsIgnoreCase(Constants.CONTAINER_NUMBER)) {
            if (StringUtils.isEmpty(cellValue)) {
                throw new ValidationException("Container Number is missing in Line: " + i + ", Please enter and re-upload.");
            }
            if (!existingContainerNumberSet.contains(cellValue)) {
                throw new ValidationException("Container number " + cellValue + " is not present in consolidation at row: " + i);
            }
            containerNumberList.add(cellValue);
        }
    }

    private void checkForContainerCodeValidation(Map<String, Set<String>> masterListsMap,
                                                 String cellValue, int rowNum, List<String> errorList) throws ValidationException {
        if (!(cellValue.isEmpty() && ShipmentVersionContext.isV3())
                && masterListsMap.containsKey(Constants.CONTAINER_TYPES)
                && !masterListsMap.get(Constants.CONTAINER_TYPES).contains(cellValue)) {
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Type", cellValue));
        }
    }

    private void checkForDuplicateContainerNumberValidation(int guidPos, Row row, String containerNumber,
                                                            int rowNum, boolean isUpdate,
                                                            Map<String, String> existingContainerNumbers) throws ValidationException {
        String guid = guidPos == -1 ? "" : getCellValueAsString(row.getCell(guidPos));
        if (isUpdate && !StringUtility.isEmpty(containerNumber) && existingContainerNumbers.containsKey(containerNumber)
                && !existingContainerNumbers.get(containerNumber).equals(guid)) {
            throw new ValidationException("Duplicate container number " + containerNumber + " found at row: " + rowNum + ". In a booking all container numbers must be Unique.");
        }
        if (existingContainerNumbers.containsKey(containerNumber) && !isUpdate) {
            throw new ValidationException("Duplicate container number " + containerNumber + " found at row: " + rowNum + ". In a booking all container numbers must be Unique.");
        }
        if (!StringUtils.isEmpty(containerNumber) && !existingContainerNumbers.containsKey(containerNumber)) {
            existingContainerNumbers.put(containerNumber, guid);
        }
    }

    private void checkForValueValidations(String column, Map<String, Set<String>> masterListsMap, String cellValue, int rowNum, String transportMode, List<String> errorList) throws ValidationException {
        negativeFieldValidator(column, Constants.GROSS_WEIGHT, cellValue, rowNum, "Cargo Wt.", errorList);
        negativeFieldValidator(column, Constants.GROSS_VOLUME, cellValue, rowNum, "Vol.", errorList);
        negativeFieldValidator(column, "measurement", cellValue, rowNum, "Measurement", errorList);
        negativeFieldValidator(column, Constants.NET_WEIGHT, cellValue, rowNum, "Gr. Wt.", errorList);
        negativeFieldValidator(column, "tareWeight", cellValue, rowNum, "Tare Wt.", errorList);
        masterdataValidation(cellValue, column, "commodityGroup", masterListsMap, MasterDataType.COMMODITY_GROUP.getDescription(), rowNum, "Commodity Category", errorList);
        fieldLengthValidation(column, "carrierSealNumber", cellValue, 100, rowNum, "Carrier Seal", errorList);
        fieldLengthValidation(column, "customsSealNumber", cellValue, 100, rowNum, "Customs Seal", errorList);
        fieldLengthValidation(column, "shipperSealNumber", cellValue, 100, rowNum, "Shipper Seal", errorList);
        fieldLengthValidation(column, "veterinarySealNumber", cellValue, 100, rowNum, "Veterinary Seal", errorList);
        fieldLengthValidation(column, "marksNums", cellValue, 25000, rowNum, "Marks & Numbers", errorList);
        fieldLengthValidation(column, "descriptionOfGoods", cellValue, 25000, rowNum, "Description", errorList);
        fieldLengthValidation(column, "containerComments", cellValue, 255, rowNum, "Container Comments", errorList);
        fieldLengthValidation(column, "handlingInfo", cellValue, 255, rowNum, "Handling Info", errorList);
        checkPacksValidations(column, cellValue, rowNum, transportMode, errorList);
    }

    private void negativeFieldValidator(String column, String actualColumn, String cellValue, int rowNum, String msgContent, List<String> errorList) {
        if (column.equalsIgnoreCase(actualColumn) && cellValue.contains("-")) {
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, msgContent, cellValue));
        }
    }

    private void checkPacksValidations(String column, String cellValue, int rowNum, String transportMode, List<String> errorList) {
        if (!column.equalsIgnoreCase(Constants.PACKS)) {
            return; // not relevant column
        }

        // Skip validation if empty and V3
        if ((cellValue == null || cellValue.isEmpty()) && ShipmentVersionContext.isV3()) {
            return;
        }

        try {
            // Check for Air transport mode with invalid packs
            if (Constants.TRANSPORT_MODE_AIR.equals(transportMode) &&
                    (cellValue == null || Integer.parseInt(cellValue) < 1)) {
                errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Packages", cellValue));
            }

            // General numeric validation
            int t = Integer.parseInt(StringUtility.convertToString(cellValue));
            if (t < 1) {
                errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Packages", cellValue));
            }

        } catch (NumberFormatException e) {
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Packages", cellValue));
        }
    }

    private void checkForUnitValidations(Map<String, Set<String>> masterListsMap, String column, String cellValue, int rowNum, String transportMode, List<String> errorList)
            throws ValidationException {
        if (column.toLowerCase().contains("dgsubstanceid") && !StringUtils.isEmpty(cellValue) &&
                masterListsMap.containsKey("DGSubstanceUNDGContact") && !masterListsMap.get("DGSubstanceUNDGContact").contains(cellValue)) {
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "DG Substance Id", cellValue));
        }
        if (column.equalsIgnoreCase("innerpackagetype") && !cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.PACKS_UNIT.getDescription()) && !masterListsMap.get(MasterDataType.PACKS_UNIT.getDescription()).contains(cellValue)) {
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Inner package type", cellValue));
        }
        validateMeasurementUnit(masterListsMap, column, cellValue, rowNum, errorList);
        if (column.toLowerCase().contains("volumeunit")) {
            validateVolumeUnit(masterListsMap, column, cellValue, rowNum);
        }
        validateContainers(masterListsMap, column, cellValue, rowNum, transportMode, errorList);

        if (column.toLowerCase().contains("chargeableunit") && !cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.WEIGHT_UNIT.getDescription())
                && !masterListsMap.get(MasterDataType.WEIGHT_UNIT.getDescription()).contains(cellValue)) {
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Chargeable unit", cellValue));
        }

        validateLengthWidhtHeightUnit(masterListsMap, column, cellValue, rowNum, errorList);

        validateCountryCode(masterListsMap, column, cellValue, rowNum, errorList);
    }

    private void validateContainers(Map<String, Set<String>> masterListsMap, String column, String cellValue, int rowNum, String transportMode, List<String> errorList) {
        if (column.equalsIgnoreCase("containercode") && cellValue.isEmpty() && !transportMode.equals(Constants.TRANSPORT_MODE_AIR)) {
            errorList.add(String.format(ContainerConstants.GENERIC_MANDATORY_FIELD_MSG, rowNum, "Type"));
        }

        if (column.toLowerCase().contains("containercode") && !cellValue.isEmpty() && masterListsMap.containsKey(Constants.CONTAINER_TYPES) && !masterListsMap.get(Constants.CONTAINER_TYPES).contains(cellValue)) {
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Type", cellValue));
        }
    }

    private void validateCountryCode(Map<String, Set<String>> masterListsMap, String column, String cellValue, int rowNum, List<String> errorList) {
        if (column.toLowerCase().contains("countrycode") && !cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.COUNTRIES.getDescription()) &&
                !masterListsMap.get(MasterDataType.COUNTRIES.getDescription()).contains(cellValue)) {
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Country Code", cellValue));
        }
    }

    private void validateLengthWidhtHeightUnit(Map<String, Set<String>> masterListsMap, String column, String cellValue, int rowNum, List<String> errorList) {
        if (column.toLowerCase().contains("volumetricweightunit") && !cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.WEIGHT_UNIT.getDescription()) && !masterListsMap.get(MasterDataType.WEIGHT_UNIT.getDescription()).contains(cellValue)) {
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Volumetric weight unit", cellValue));
        }
        if (column.toLowerCase().contains("lengthunit") && !cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.DIMENSION_UNIT.getDescription()) &&
                !masterListsMap.get(MasterDataType.DIMENSION_UNIT.getDescription()).contains(cellValue)) {
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Length unit", cellValue));
        }
        if (column.toLowerCase().contains("widthunit") && !cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.DIMENSION_UNIT.getDescription()) &&
                !masterListsMap.get(MasterDataType.DIMENSION_UNIT.getDescription()).contains(cellValue)) {
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Width unit", cellValue));
        }

        if (column.toLowerCase().contains("heightunit") && !cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.DIMENSION_UNIT.getDescription()) &&
                !masterListsMap.get(MasterDataType.DIMENSION_UNIT.getDescription()).contains(cellValue)) {
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Height unit", cellValue));
        }
    }

    private void validateMeasurementUnit(Map<String, Set<String>> masterListsMap, String column, String cellValue, int rowNum, List<String> errorList) {
        if (column.toLowerCase().contains("measurementunit")) {
            if (column.equalsIgnoreCase("innerpackagemeasurementunit") && !cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.DIMENSION_UNIT.getDescription()) &&
                    !masterListsMap.get(MasterDataType.DIMENSION_UNIT.getDescription()).contains(cellValue)) {
                errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Inner package meaurement unit", cellValue));
            }
            if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.DIMENSION_UNIT.getDescription()) &&
                    !masterListsMap.get(MasterDataType.DIMENSION_UNIT.getDescription()).contains(cellValue)) {
                errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, rowNum, "Measurement unit", cellValue));
            }
        }
    }

    private void validateVolumeUnit(Map<String, Set<String>> masterListsMap, String column, String cellValue, int rowNum) {
        switch (column.toLowerCase()) {
            case "allocatedvolumeunit": {
                if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.VOLUME_UNIT.getDescription()) &&
                        !masterListsMap.get(MasterDataType.VOLUME_UNIT.getDescription()).contains(cellValue)) {
                    throw new ValidationException("Allocated Volume unit is null or invalid at row: " + rowNum);
                }
                break;
            }
            case "achievedvolumeunit": {
                if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.VOLUME_UNIT.getDescription()) &&
                        !masterListsMap.get(MasterDataType.VOLUME_UNIT.getDescription()).contains(cellValue)) {
                    throw new ValidationException("Achieved Volume unit is null or invalid at row: " + rowNum);
                }
                break;
            }
            case "volumeunit": {
                if (!cellValue.isEmpty() && masterListsMap.containsKey(MasterDataType.VOLUME_UNIT.getDescription()) &&
                        !masterListsMap.get(MasterDataType.VOLUME_UNIT.getDescription()).contains(cellValue)) {
                    throw new ValidationException("Vol. Unit is null or invalid at row: " + rowNum);
                }
                break;
            }
            default:
                log.debug(Constants.SWITCH_DEFAULT_CASE_MSG, column.toLowerCase());
                break;
        }
    }

    private String getCellValueAsString(Cell cell) {
        if (cell == null) return "";
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

    private void validateExcel(Sheet sheet) {

        if (sheet == null || sheet.getPhysicalNumberOfRows() <= 0) {
            throw new ValidationException(ContainerConstants.EMPTY_EXCEL_SHEET_V3);
        }
    }

    private void setField(T entity, String attributeName, String attributeValue, int rowNum) {

        Map<String, Field> fieldMap = new HashMap<>();
        for (Field v : entity.getClass().getDeclaredFields()) {
            fieldMap.put(v.getName(), v);
        }
        if (entity.getClass().getSuperclass() != null) {
            for (Field v : entity.getClass().getSuperclass().getDeclaredFields()) { // multenancy
                fieldMap.put(v.getName(), v);
            }
            if (entity.getClass().getSuperclass().getSuperclass() != null) {
                for (Field v : entity.getClass().getSuperclass().getSuperclass().getDeclaredFields()) { //bas entity
                    fieldMap.put(v.getName(), v);
                }
            }
        }

        Field field = fieldMap.get(attributeName);
        if (field == null) {
            return;
        }
        field.setAccessible(true);

        setParsedValueInField(entity, attributeName, attributeValue, rowNum, field);
    }

    private void setParsedValueInField(T entity, String attributeName, String attributeValue, int rowNum, Field field) {
        Class<?> fieldType = field.getType();
        Object parsedValue = null;
        try {
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
            } else if (fieldType == UUID.class) {
                parsedValue = UUID.fromString(attributeValue);
            } else {
                throw new NoSuchFieldException();
            }

            field.set(entity, parsedValue);
        } catch (Exception ex) {
            raiseException(attributeName, rowNum, fieldType);
        }
    }

    private void raiseException(String attributeName, int rowNum, Class<?> fieldType) {
        if (fieldType == Long.class || fieldType == long.class) {
            throw new ValidationException(attributeName.toUpperCase() + " is invalid at row: " + rowNum + ". Please provide integer value and within the range of integer");
        }
        throw new ValidationException(attributeName + " is invalid at row: " + rowNum + ". Please provide correct value");
    }

    public void setFieldForEvents(T entity, String attributeName, String attributeValue) throws NoSuchFieldException, IllegalAccessException {
        if (attributeName.equals("containerNumber"))
            return;
        if (attributeName.equals("publicTrackingEvent"))
            attributeName = "isPublicTrackingEvent";
        Field field = entity.getClass().getDeclaredField(attributeName);
        field.setAccessible(true);
        DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME;

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
            parsedValue = LocalDateTime.parse(attributeValue.trim(), formatter);
        } else {
            throw new NoSuchFieldException();
        }

        field.set(entity, parsedValue);
    }

    public Map<String, Set<String>> getAllMasterDataPacking(List<String> unlocationsList, List<String> commodityCodesList, Map<String, Set<String>> masterDataMap, Map<String, String> locCodeToLocationReferenceGuidMap) {
        var weightUnitMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.WEIGHT_UNIT, masterDataMap)), executorService);
        var volumeUnitMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.VOLUME_UNIT, masterDataMap)), executorService);
        var temperatureUnitMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.TEMPERATURE_UNIT, masterDataMap)), executorService);
        var dimensionUnitMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.DIMENSION_UNIT, masterDataMap)), executorService);
        var dgClassMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.DG_CLASS, masterDataMap)), executorService);
        var countryCodeMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.COUNTRIES, masterDataMap)), executorService);
        var packUnitMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.PACKS_UNIT, masterDataMap)), executorService);
        var unlocationMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchUnlocationData(unlocationsList, masterDataMap, locCodeToLocationReferenceGuidMap)), executorService);
        var commodityMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchCommodityData(commodityCodesList, masterDataMap)), executorService);

        CompletableFuture.allOf(unlocationMasterData, weightUnitMasterData, volumeUnitMasterData, temperatureUnitMasterData, countryCodeMasterData, dimensionUnitMasterData, dgClassMasterData, packUnitMasterData, commodityMasterData).join();

        return masterDataMap;
    }

    public Map<String, Set<String>> getAllMasterDataContainer(List<String> unlocationsList, List<String> commodityCodesList, Map<String, Set<String>> masterDataMap, Map<String, String> locCodeToLocationReferenceGuidMap) {
        var weightUnitMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.WEIGHT_UNIT, masterDataMap)), executorService);
        var volumeUnitMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.VOLUME_UNIT, masterDataMap)), executorService);
        var temperatureUnitMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.TEMPERATURE_UNIT, masterDataMap)), executorService);
        var hblModeMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.HBL_DELIVERY_MODE, masterDataMap)), executorService);
        var dimensionUnitMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.DIMENSION_UNIT, masterDataMap)), executorService);
        var dgClassMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.DG_CLASS, masterDataMap)), executorService);
        var packUnitMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.PACKS_UNIT, masterDataMap)), executorService);
        var containerTypeMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchContainerType(masterDataMap)), executorService);
        var commodityGroupData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.COMMODITY_GROUP, masterDataMap)), executorService);
        var packingGroupData = CompletableFuture.runAsync(withMdc(() -> this.fetchMasterLists(MasterDataType.PACKING_GROUP, masterDataMap)), executorService);
        var unlocationMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchUnlocationData(unlocationsList, masterDataMap, locCodeToLocationReferenceGuidMap)), executorService);
        var commodityMasterData = CompletableFuture.runAsync(withMdc(() -> this.fetchCommodityData(commodityCodesList, masterDataMap)), executorService);

        CompletableFuture.allOf(weightUnitMasterData, volumeUnitMasterData, temperatureUnitMasterData, hblModeMasterData, dimensionUnitMasterData, dgClassMasterData, packUnitMasterData, containerTypeMasterData, packingGroupData, commodityGroupData, unlocationMasterData, commodityMasterData).join();

        return masterDataMap;
    }

    private Map<String, Set<String>> getAllMasterDataEvents(Map<String, Set<String>> masterDataMap) {
        this.fetchMasterLists(MasterDataType.ORDER_EVENTS, masterDataMap);
        return masterDataMap;
    }

    public void fetchMasterLists(MasterDataType masterDataType, Map<String, Set<String>> masterDataMap) {
        try {
            CommonV1ListRequest request = new CommonV1ListRequest();
            List<Object> field = new ArrayList<>(List.of("ItemType"));
            String operator = "=";
            List<Object> criteria = List.of(field, operator, masterDataType.getId());
            request.setCriteriaRequests(criteria);
            V1DataResponse v1DataResponse = v1Service.fetchMasterData(request);
            if (v1DataResponse != null && v1DataResponse.entities instanceof List<?>) {
                List<MasterData> masterDataList = jsonHelper.convertValueToList(v1DataResponse.entities, MasterData.class);
                if (masterDataList != null && !masterDataList.isEmpty()) {
                    Set<String> masterDataSet = masterDataList.stream().filter(Objects::nonNull).map(MasterData::getItemValue).collect(Collectors.toSet());
                    masterDataMap.put(masterDataType.getDescription(), masterDataSet);
                }
            }
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: fetchMasterLists in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CSVParsingUtil.class.getSimpleName(), ex.getMessage());
        }
    }

    public void fetchContainerType(Map<String, Set<String>> masterDataMap) {
        try {
            CommonV1ListRequest request = new CommonV1ListRequest();
            V1DataResponse v1DataResponse = v1Service.fetchContainerTypeData(request);
            if (v1DataResponse != null && v1DataResponse.entities instanceof List<?>) {
                List<V1ContainerTypeResponse> containerTypeList = jsonHelper.convertValueToList(v1DataResponse.entities, V1ContainerTypeResponse.class);
                if (containerTypeList != null && !containerTypeList.isEmpty()) {
                    Set<String> containerTypeSet = containerTypeList.stream().filter(Objects::nonNull).map(V1ContainerTypeResponse::getCode).collect(Collectors.toSet());
                    masterDataMap.put(Constants.CONTAINER_TYPES, containerTypeSet);
                }
            }
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: fetchContainerType in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CSVParsingUtil.class.getSimpleName(), ex.getMessage());
        }
    }

    public void fetchUnlocationData(List<String> unlocationsList, Map<String, Set<String>> masterDataMap, Map<String, String> locCodeToLocationReferenceGuidMap) {
        try {
            CommonV1ListRequest request = new CommonV1ListRequest();
            if (unlocationsList.isEmpty()) {
                return;
            }
            List<Object> field = new ArrayList<>(List.of("LocCode"));
            String operator = Operators.IN.getValue();
            List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(unlocationsList)));
            request.setCriteriaRequests(criteria);
            V1DataResponse v1DataResponse = v1Service.fetchUnlocation(request);
            if (v1DataResponse != null && v1DataResponse.entities instanceof List<?>) {
                List<UnlocationsResponse> unlocationList = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
                if (unlocationList != null && !unlocationList.isEmpty()) {
                    Set<String> unlocationSet = unlocationList.stream().filter(Objects::nonNull).map(UnlocationsResponse::getLocCode).collect(Collectors.toSet());
                    locCodeToLocationReferenceGuidMap.putAll(unlocationList.stream().filter(Objects::nonNull).collect(Collectors.toMap(UnlocationsResponse::getLocCode, UnlocationsResponse::getLocationsReferenceGUID)));
                    masterDataMap.put(Constants.UNLOCATIONS, unlocationSet);
                }
            }
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: fetchUnlocationData in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CSVParsingUtil.class.getSimpleName(), ex.getMessage());
        }
    }

    public void fetchCommodityData(List<String> commodityCodesList, Map<String, Set<String>> masterDataMap) {
        try {
            if (commodityCodesList.isEmpty())
                return;
            V1DataResponse response = getCommodityDataResponse(commodityCodesList);
            if (response != null && response.entities instanceof List<?>) {
                List<CommodityResponse> commodityList = jsonHelper.convertValueToList(response.entities, CommodityResponse.class);
                if (commodityList != null && !commodityList.isEmpty()) {
                    Set<String> commoditySet = commodityList.stream().filter(Objects::nonNull).map(CommodityResponse::getCode).collect(Collectors.toSet());
                    masterDataMap.put("CommodityCodes", commoditySet);
                }
            }
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: fetchCommodityData in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CSVParsingUtil.class.getSimpleName(), ex.getMessage());
        }
    }

    public V1DataResponse getCommodityDataResponse(List<String> commodityCodesList) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of("Code"));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(commodityCodesList)));
        request.setCriteriaRequests(criteria);
        return v1Service.fetchCommodityData(request);
    }

    public void setUNDGContactMasterDataAndFlashPointMasterData(List<Long> dgSubstanceIdList,
                                                                Map<Long, Long> undg, Map<Long, String> flashpoint) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        if (dgSubstanceIdList.isEmpty())
            return;
        List<Object> field = new ArrayList<>(List.of("Id"));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(dgSubstanceIdList)));
        request.setCriteriaRequests(criteria);
        V1DataResponse response = v1Service.fetchDangerousGoodData(request);
        if (response != null && response.entities instanceof List<?>) {
            List<EntityTransferDGSubstance> list = jsonHelper.convertValueToList(response.entities, EntityTransferDGSubstance.class);
            if (list != null && !list.isEmpty()) {
                undg.putAll(list.stream().filter(Objects::nonNull).collect(Collectors.toMap(EntityTransferDGSubstance::getId, EntityTransferDGSubstance::getUNIDNo)));
                flashpoint.putAll(list.stream().filter(Objects::nonNull).collect(Collectors.toMap(EntityTransferDGSubstance::getId, EntityTransferDGSubstance::getFlashPoint)));
            }
        }
    }


    public Runnable withMdc(Runnable runnable) {
        Map<String, String> mdc = MDC.getCopyOfContextMap();
        String token = RequestAuthContext.getAuthToken();
        return () -> {
            try {
                MDC.setContextMap(mdc);
                RequestAuthContext.setAuthToken(token);
                runnable.run();
            } finally {
                MDC.clear();
                RequestAuthContext.removeToken();
            }
        };
    }

}
