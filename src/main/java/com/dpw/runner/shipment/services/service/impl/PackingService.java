package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.AutoCalculatePackingRequest;
import com.dpw.runner.shipment.services.dto.request.PackingExcelModel;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.response.AutoCalculatePackingResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.syncing.Entity.BulkPackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingSync;
import com.dpw.runner.shipment.services.utils.*;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Slf4j
@Service
public class PackingService implements IPackingService {
    @Autowired
    IPackingDao packingDao;

    @Autowired
    @Lazy
    private IConsolidationService consolidationService;

    @Autowired
    IContainerDao containersDao;
    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDao;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private IPackingSync packingSync;
    @Lazy
    @Autowired
    private ISyncQueueService syncQueueService;
    @Autowired
    private SyncConfig syncConfig;

    @Autowired
    private IContainerService containerService;

    @Autowired
    private CSVParsingUtil<Packing> parser;
    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private CommonUtils commonUtils;

    private List<String> columnsSequenceForExcelDownloadForCargo = List.of(
            "guid", "shipmentNumber", "packs", "packsType", "innerPackageNumber", "innerPackageType", "origin", "packingOrder",
            "length", "lengthUnit", "width", "widthUnit", "height", "heightUnit", "weight", "weightUnit", "volume", "volumeUnit",
            "netWeight", "netWeightUnit", "chargeable", "chargeableUnit", "marksnNums", "countryCode", "goodsDescription",
            "referenceNumber", "inspections", "DGClass", "DGSubstanceId", "flashPoint", "UNDGContact", "isTemperatureControlled",
            "minTemp", "minTempUnit", "maxTemp", "maxTempUnit", "commodity", "HSCode", "customsReleaseCode"
    );

    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        PackingRequest request = null;
        request = (PackingRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Packing create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Packing packing = convertRequestToEntity(request);
        try {
            packing = packingDao.save(packing);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(packing)
                            .prevData(null)
                            .parent(Packing.class.getSimpleName())
                            .parentId(packing.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            log.info("Packing Details created successfully for Id {} with Request Id {}", packing.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(packing));
    }

    @Override
    public void uploadPacking(BulkUploadRequest request) throws RunnerException {
        if (request.getConsolidationId() == null) {
            throw new ValidationException("Please save the consolidation and then try again.");
        }

        List<Packing> packings = packingDao.findByConsolidationId(request.getConsolidationId());
        var packingsMap = packings.stream().collect(Collectors.toMap(Packing::getGuid, Function.identity()));

        Map<Long, Long> dicDGSubstanceUNDGContact = new HashMap<>();
        Map<Long, String> dicDGSubstanceFlashPoint = new HashMap<>();
        Map<String, Set<String>> masterDataMap = new HashMap<>();
        List<Packing> packingList = new ArrayList<>();
        try {
            packingList = parser.parseExcelFile(request.getFile(), request, null, masterDataMap, Packing.class, PackingExcelModel.class, dicDGSubstanceUNDGContact, dicDGSubstanceFlashPoint);
            packingList.stream().forEach(packing -> {
                packing.setConsolidationId(request.getConsolidationId());
            });
        }
        catch(IOException e) {
            throw new RunnerException(e.getMessage());
        }

        applyPackingValidations(packingList, request, masterDataMap, dicDGSubstanceUNDGContact, dicDGSubstanceFlashPoint);

        packingList = packingDao.saveAll(packingList);
        packingSync.sync(packingList, request.getConsolidationId(), request.getShipmentId());
    }

    private void applyPackingValidations(List<Packing> packingList, BulkUploadRequest request, Map<String, Set<String>> masterDataMap,
                                         Map<Long, Long> dicDGSubstanceUNDGContact, Map<Long, String> dicDGSubstanceFlashPoint
    ) throws RunnerException {
        String transportMode = request.getTransportMode();
        Set<String> dicCommodityType = masterDataMap.get("CommodityCodes");
        Set<String> hazardousClassMasterData = masterDataMap.get(MasterDataType.DG_CLASS.getDescription());
        for (int row = 0; row < packingList.size(); row++) {
            Packing packingRow = packingList.get(row);
            checkCalculatedVolumeAndActualVolume(row + 1, packingRow);
            applyChargeableValidation(transportMode, row + 1, packingRow, masterDataMap);
            applyCommodityTypeValidation(dicCommodityType, row + 1, packingRow);
            applyVolumetricWeightValidation(row + 1, packingRow);
            applyHazardousValidation(hazardousClassMasterData, dicDGSubstanceUNDGContact, dicDGSubstanceFlashPoint, row + 1, packingRow);
            if (!StringUtils.isEmpty(packingRow.getFlashPoint()) && packingRow.getDGSubstanceId() == null) {
                throw new ValidationException(PackingConstants.FLASH_POINT_INVALID_ERROR + row + 1);
            }
        }
    }

    private void applyVolumetricWeightValidation(int i, Packing packingRow) throws RunnerException {
        if (!StringUtils.isEmpty(packingRow.getVolumeWeightUnit())) {
            if (packingRow.getVolumeWeight() != null) {
                String wtunit = packingRow.getWeightUnit() != null ? packingRow.getWeightUnit() : WEIGHT_UNIT_KG;
                var vwob = consolidationService.calculateVolumeWeight(Constants.TRANSPORT_MODE_AIR,
                        packingRow.getWeightUnit() == null ? Constants.WEIGHT_UNIT_KG : packingRow.getWeightUnit(),
                        packingRow.getVolumeUnit() == null ? Constants.VOLUME_UNIT_M3 : packingRow.getVolumeUnit(),
                        packingRow.getWeight() == null ? BigDecimal.ZERO : packingRow.getWeight(),
                        packingRow.getVolume() == null ? BigDecimal.ZERO : packingRow.getVolume());
                if (vwob.getVolumeWeight() != null) {
                    var calculatedVolumeWeight = vwob.getVolumeWeight();
                    calculatedVolumeWeight = calculatedVolumeWeight.setScale(2, BigDecimal.ROUND_HALF_UP);
                    var actualVolumeWeight = packingRow.getVolumeWeight();
                    actualVolumeWeight = actualVolumeWeight.setScale(2, BigDecimal.ROUND_HALF_UP);
                    if (!wtunit.equals(vwob.getVolumeWeightUnit())) {
                        throw new ValidationException("Volumetric weight unit not in " + wtunit + " at row: " + i);
                    }
                    if (calculatedVolumeWeight.compareTo(actualVolumeWeight) != 0) { // not equal
                        throw new ValidationException("Volumetric weight is invalid at row: " + i);
                    }
                }
            }
        } else if (packingRow.getVolumeWeight() != null && StringUtils.isEmpty(packingRow.getVolumeWeightUnit())) {
            throw new ValidationException("Volumetric weight unit is empty or Volumetric weight unit not entered at row: " + i);
        }
    }


    private static void applyHazardousValidation(Set<String> hazardousClassMasterData,
                                                 Map<Long, Long> dicDGSubstanceUNDGContact, Map<Long, String> dicDGSubstanceFlashPoint
            , int row, Packing packingRow) {
        Boolean isHazardous = packingRow.getHazardous();
        if (isHazardous != null) {
            if (isHazardous == true) {
                // DG CLASS(HAZARDOUS CLASS)
                if (!StringUtils.isEmpty(packingRow.getDGClass())) {
                    String dgClass = packingRow.getDGClass();
                    if (!StringUtils.isEmpty(dgClass)) {
                        if (hazardousClassMasterData != null && !hazardousClassMasterData.contains(dgClass)) {
                            throw new ValidationException("DG class is invalid at row: " + row);
                        }
                    }
                }

                if (packingRow.getDGSubstanceId() != null) {
                    if (!dicDGSubstanceUNDGContact.containsKey(packingRow.getDGSubstanceId())) {
                        throw new ValidationException("DG Substance Id is invalid at row: " + row);
                    }
                }

                if (!StringUtils.isEmpty(packingRow.getFlashPoint())) {
                    if (packingRow.getDGSubstanceId() != null) {
                        if (!dicDGSubstanceFlashPoint.containsKey(packingRow.getDGSubstanceId()) ||
                                dicDGSubstanceFlashPoint.get(packingRow.getDGSubstanceId()) != packingRow.getFlashPoint()) {
                            throw new ValidationException(PackingConstants.FLASH_POINT_INVALID_ERROR + row);
                        }
                    } else if (packingRow.getDGSubstanceId() == null && !StringUtils.isEmpty(packingRow.getFlashPoint())) {
                        throw new ValidationException(PackingConstants.FLASH_POINT_INVALID_ERROR + row);
                    }
                }

                if (!StringUtils.isEmpty(packingRow.getUNDGContact())) {
                    if (packingRow.getDGSubstanceId() != null) {
                        long substanceId = packingRow.getDGSubstanceId();
                        if (!dicDGSubstanceUNDGContact.containsKey(substanceId) ||
                                dicDGSubstanceUNDGContact.get(packingRow.getDGSubstanceId()) != Long.valueOf(packingRow.getUNDGContact())) {
                            throw new ValidationException("UNDGContact is invalid at row: " + row);
                        }
                    } else if (packingRow.getDGSubstanceId() == null && !StringUtils.isEmpty(packingRow.getUNDGContact())) {
                        throw new ValidationException("UNDGContact is invalid at row: " + row);
                    }
                }
            }
        }
    }

    private static void applyCommodityTypeValidation(Set<String> dicCommodityType, int row, Packing packingRow) {
        String commodityType = packingRow.getCommodity();
        commodityType = commodityType == null ? StringUtils.EMPTY : commodityType.trim();
        if (!StringUtils.isEmpty(commodityType)) {
            if (dicCommodityType != null && dicCommodityType.contains(commodityType) == false) {
                throw new ValidationException("Commodity Type " + commodityType + " is not valid at row " + row);
            }
        }
    }

    private void checkCalculatedVolumeAndActualVolume(int row, Packing packingRow) throws RunnerException {
        if (!StringUtils.isEmpty(packingRow.getVolumeUnit())) {
            if (packingRow.getVolume() != null) {
                if (packingRow.getVolumeUnit() != VOLUME_UNIT_M3) {
                    throw new ValidationException("Volume unit not in M3 at row: " + row);
                }
                BigDecimal actualVolume = packingRow.getVolume();
                actualVolume = actualVolume.setScale(2, BigDecimal.ROUND_HALF_UP);
                var calculatedVolume = getCalculatedVolume(packingRow);
                calculatedVolume = calculatedVolume.setScale(2, BigDecimal.ROUND_HALF_UP);
                if (actualVolume.compareTo(calculatedVolume) != 0) { // not equal
                    throw new ValidationException("Volume is invalid at row: " + row);
                }
            }
        } else if (packingRow.getVolume() != null && StringUtils.isEmpty(packingRow.getVolumeUnit())) {
            throw new ValidationException("Volume unit is empty or Volume unit not entered at row: " + row);
        }
    }

    private BigDecimal getCalculatedVolume(Packing packingRow) throws RunnerException {
        if (!StringUtils.isEmpty(packingRow.getPacks()) && packingRow.getLength() != null
                && packingRow.getHeight() != null && packingRow.getWidth() != null
                && !StringUtils.isEmpty(packingRow.getWidthUnit()) && !StringUtils.isEmpty(packingRow.getHeightUnit())
                && !StringUtils.isEmpty(packingRow.getHeightUnit())) {
            String lengthUnit = packingRow.getLengthUnit().equals(FT) ? FOOT_FT : packingRow.getLengthUnit();
            String widthUnit = packingRow.getWidthUnit().equals(FT) ? FOOT_FT : packingRow.getWidthUnit();
            String heightUnit = packingRow.getHeightUnit().equals(FT) ? FOOT_FT : packingRow.getHeightUnit();
            var length = new BigDecimal(convertUnit(LENGTH, packingRow.getLength(), lengthUnit, M).doubleValue());
            var height = new BigDecimal(convertUnit(LENGTH, packingRow.getHeight(), heightUnit, M).doubleValue());
            var width = new BigDecimal(convertUnit(LENGTH, packingRow.getWidth(), widthUnit, M).doubleValue());
            int packs = Integer.parseInt(packingRow.getPacks());
            return length.multiply(width).multiply(height).multiply(BigDecimal.valueOf(packs));
        }
        return null;
    }

    private void applyChargeableValidation(String transportMode, int row, Packing packingRow, Map<String, Set<String>> masterDataMap) throws RunnerException {
        if (!StringUtils.isEmpty(packingRow.getChargeableUnit())) {
            if (masterDataMap.containsKey(MasterDataType.WEIGHT_UNIT.getDescription()) &&
                    !masterDataMap.get(MasterDataType.WEIGHT_UNIT.getDescription()).contains(packingRow.getChargeableUnit()))
                throw new ValidationException("Chargeable unit is invalid at row: " + row);
        }
        if (packingRow.getChargeableUnit() != null && !packingRow.getChargeableUnit().isEmpty() &&
                transportMode != null && transportMode.equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR) &&
                packingRow.getChargeable() != null) {
            if (!packingRow.getChargeableUnit().equals(Constants.WEIGHT_UNIT_KG)) {
                throw new ValidationException("Chargeable unit not in KG at row: " + row);
            }
            var actualChargeable = packingRow.getChargeable();
            actualChargeable = actualChargeable.setScale(2, BigDecimal.ROUND_HALF_UP);
            BigDecimal calculatedChargeable = null;

            var vwob = consolidationService.calculateVolumeWeight(Constants.TRANSPORT_MODE_AIR,
                    packingRow.getWeightUnit() == null ? Constants.WEIGHT_UNIT_KG : packingRow.getWeightUnit(),
                    packingRow.getVolumeUnit() == null ? Constants.VOLUME_UNIT_M3 : packingRow.getVolumeUnit(),
                    packingRow.getWeight() == null ? BigDecimal.ZERO : packingRow.getWeight(),
                    packingRow.getVolume() == null ? BigDecimal.ZERO : packingRow.getVolume());
            if (vwob.getChargeable() != null) {
                calculatedChargeable = vwob.getChargeable();
                calculatedChargeable = calculatedChargeable.setScale(2, BigDecimal.ROUND_HALF_UP);
                if (calculatedChargeable != actualChargeable) {
                    BigDecimal difference = calculatedChargeable.subtract(actualChargeable).abs();
                    BigDecimal threshold = new BigDecimal("0.01");
                    if (difference.compareTo(threshold) > 0) {
                        throw new ValidationException("Chargeable is invalid at row: " + row);
                    }
                }
            }
        } else if (packingRow.getChargeable() != null && StringUtils.isEmpty(packingRow.getChargeableUnit())) {
            throw new ValidationException("Chargeable unit is empty or Chargeable unit not entered at row: " + row);
        }
    }

    @Override
    public void downloadPacking(HttpServletResponse response, BulkDownloadRequest request) throws RunnerException {
        try {
            List<Packing> result = new ArrayList<>();
            if (request.getShipmentId() != null) {
                ListCommonRequest req = constructListCommonRequest("shipmentId", Long.valueOf(request.getShipmentId()), "=");
                Pair<Specification<Packing>, Pageable> pair = fetchData(req, Packing.class);
                Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
                List<Packing> packingList = packings.getContent();
                result.addAll(packingList);
            }

            if (request.getConsolidationId() != null) {
                ListCommonRequest req2 = constructListCommonRequest("consolidationId", Long.valueOf(request.getConsolidationId()), "=");
                Pair<Specification<Packing>, Pageable> pair = fetchData(req2, Packing.class);
                Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
                List<Packing> packingList = packings.getContent();
                if (result.isEmpty()) {
                    result.addAll(packingList);
                } else {
                    result = result.stream().filter(result::contains).collect(Collectors.toList());
                }
            }
            LocalDateTime currentTime = LocalDateTime.now();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(YYYY_MM_DD_HH_MM_SS_FORMAT);
            String timestamp = currentTime.format(formatter);
            String filenameWithTimestamp = "CargoDetails_" + timestamp + Constants.XLSX;

            try(XSSFWorkbook workbook = new XSSFWorkbook()) {
                XSSFSheet sheet = workbook.createSheet("CargoDetails");

                List<PackingExcelModel> modelList = commonUtils.convertToList(result, PackingExcelModel.class);
                convertModelToExcel(modelList, sheet, request);

                response.setContentType(CONTENT_TYPE_FOR_EXCEL);
                response.setHeader("Content-Disposition", "attachment; filename=" + filenameWithTimestamp);

                try (var outputStream = response.getOutputStream()) {
                    workbook.write(outputStream);
                }
            }

        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }
    private void convertModelToExcel(List<PackingExcelModel> modelList, XSSFSheet sheet, BulkDownloadRequest request) throws IllegalAccessException {

        // Create header row using annotations for order
        Row headerRow = sheet.createRow(0);
        Field[] fields = PackingExcelModel.class.getDeclaredFields();
//        Arrays.sort(fields, Comparator.comparingInt(f -> f.getAnnotation(ExcelCell.class).order()));

        Map<String, Field> fieldNameMap = Arrays.stream(fields).filter(f->f.isAnnotationPresent(ExcelCell.class)).collect(Collectors.toMap(Field::getName, c-> c));
        ColumnsToIgnore(fieldNameMap, request);

        if(fieldNameMap.containsKey("Origin")) {
            List<String> unlocationsRefGuids = new ArrayList<>();
            for (PackingExcelModel model : modelList){
                unlocationsRefGuids.add(model.getOrigin());
            }
            if(!unlocationsRefGuids.isEmpty()) {
                Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(unlocationsRefGuids, EntityTransferConstants.LOCATION_SERVICE_GUID);
                for (PackingExcelModel model : modelList){
                    if(keyMasterDataMap.containsKey(model.getOrigin())){
                        var locCode = keyMasterDataMap.get(model.getOrigin()).LocCode;
                        model.setOrigin(locCode);
                    }
                }
            }
        }
        List<Field> fieldsList = reorderFields(fieldNameMap, columnsSequenceForExcelDownloadForCargo);
        int i = 0;
        for (var field : fieldsList){
            Cell cell = headerRow.createCell(i++);
            cell.setCellValue(!field.getAnnotation(ExcelCell.class).displayName().isEmpty() ? field.getAnnotation(ExcelCell.class).displayName() : field.getName());
        }

        // Populate data
        int rowIndex = 1;
        for (PackingExcelModel model : modelList) {
            Row row = sheet.createRow(rowIndex++);
            int cellIndex = 0;
            for (Field field : fieldsList) {
                field.setAccessible(true);
                Object value = field.get(model);
                Cell cell = row.createCell(cellIndex++);
                cell.setCellValue(value != null ? value.toString() : "");
            }
        }
    }

    private void ColumnsToIgnore(Map<String, Field> fieldNameMap, BulkDownloadRequest request) {
        for(var field : Constants.ColumnsToBeDeletedForConsolidationCargo) {
            if (fieldNameMap.containsKey(field)) {
                fieldNameMap.remove(field);
            }
        }
    }
    private List<Field> reorderFields(Map<String, Field> fieldNameMap, List<String> columnsName) {
        List<Field> fields = new ArrayList<>();
        for(var field: columnsName){
            if(fieldNameMap.containsKey(field)) {
                fields.add(fieldNameMap.get(field));
                fieldNameMap.remove(field);
            }
        }
        fields.addAll(fieldNameMap.values());
        return fields;
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        PackingRequest request = (PackingRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Packing update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.debug("Request Id is null for Packing update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<Packing> oldEntity = packingDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Packing is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Packing packing = convertRequestToEntity(request);
        packing.setId(oldEntity.get().getId());

        if(packing.getGuid() != null && !oldEntity.get().getGuid().equals(packing.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            packing = packingDao.save(packing);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(packing)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Packing.class))
                            .parent(Packing.class.getSimpleName())
                            .parentId(packing.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );
            log.info("Updated the packing details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(packing));
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Packing list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Packing>, Pageable> tuple = fetchData(request, Packing.class);
            Page<Packing> packingPage = packingDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Packing list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(packingPage.getContent()),
                    packingPage.getTotalPages(),
                    packingPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Packing async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Packing>, Pageable> tuple = fetchData(request, Packing.class);
            Page<Packing> packingPage = packingDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Packing async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(
                    ResponseHelper
                            .buildListSuccessResponse(
                                    convertEntityListToDtoList(packingPage.getContent()),
                                    packingPage.getTotalPages(),
                                    packingPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        if (commonRequestModel == null) {
            log.debug("Request is empty for Packing delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if (commonRequestModel.getId() == null) {
            log.debug("Request Id is null for Packing delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Long id = commonRequestModel.getId();

        Optional<Packing> targetPacking = packingDao.findById(id);
        if (targetPacking.isEmpty()) {
            log.debug("No entity present for id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse(PackingConstants.NO_DATA);
        }
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(targetPacking.get());
            packingDao.delete(targetPacking.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Packing.class))
                            .parent(Packing.class.getSimpleName())
                            .parentId(targetPacking.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );
            log.info("Deleted packing for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse();
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Packing retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Packing retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<Packing> packing = packingDao.findById(id);
            if (packing.isEmpty()) {
                log.debug("Packing is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Packing details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            PackingResponse response = (PackingResponse) convertEntityToDto(packing.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateWeightVolumne(CommonRequestModel commonRequestModel) throws RunnerException {
        PackContainerNumberChangeRequest request = (PackContainerNumberChangeRequest) commonRequestModel.getData();
        PackContainerNumberChangeResponse response = new PackContainerNumberChangeResponse();
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        if(request.getNewContainer() == null && request.getOldContainer() == null) {
            return ResponseHelper.buildSuccessResponse(response);
        }
        ShipmentDetails shipmentDetails = null;
        try {
            if(request.getNewPack() != null)
                shipmentDetails = shipmentDao.findById(request.getNewPack().getShipmentId()).get();
            else if(request.getOldPack() != null)
                shipmentDetails = shipmentDao.findById(request.getOldPack().getShipmentId()).get();
        }
        catch (Exception e) {
            throw new RunnerException("Please send correct shipment Id in packing request");
        }
        Containers oldContainer = null;
        if(request.getOldContainer() != null)
            oldContainer = jsonHelper.convertValue(request.getOldContainer(), Containers.class);
        Containers newContainer = null;
        if(request.getNewContainer() != null)
            newContainer = jsonHelper.convertValue(request.getNewContainer(), Containers.class);
        if(shipmentSettingsDetails.getMultipleShipmentEnabled() != null && shipmentSettingsDetails.getMultipleShipmentEnabled()
        && !shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL)
                && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA))) {

            Packing newPacking = jsonHelper.convertValue(request.getNewPack(), Packing.class);
            Packing oldPacking = newPacking;
            if(request.getOldPack() != null)
                oldPacking = jsonHelper.convertValue(request.getOldPack(), Packing.class);

            if(request.getNewPack() == null) { // delete pack scenario
                oldContainer = subtractWeightVolume(oldPacking, oldContainer);
            }
            else {
                if(request.getOldContainer() != null && request.getNewContainer() != null && Objects.equals(request.getOldContainer().getId(), request.getNewContainer().getId())) {
                    newContainer = subtractWeightVolume(oldPacking, newContainer);
                    oldContainer = null;
                }
                else {
                    oldContainer = subtractWeightVolume(oldPacking, oldContainer);
                }
                newContainer = addWeightVolume(newPacking, newContainer);
            }
        }
        if(oldContainer != null) {
            containerService.calculateUtilization(oldContainer);
            response.setOldContainer(jsonHelper.convertValue(oldContainer, ContainerResponse.class));
        }
        if(newContainer != null) {
            containerService.calculateUtilization(newContainer);
            response.setNewContainer(jsonHelper.convertValue(newContainer, ContainerResponse.class));
        }
        return ResponseHelper.buildSuccessResponse(response);
    }

    public PackSummaryResponse calculatePackSummary(List<Packing> packingList, String transportMode, String containerCategory, ShipmentMeasurementDetailsDto dto) throws RunnerException {
        try {
            PackSummaryResponse response = new PackSummaryResponse();
            double totalWeight = 0;
            double netWeight = 0;
            StringBuilder packsCount = new StringBuilder();
            double volumeWeight = 0;
            double volumetricWeight = 0;
            double chargeableWeight = 0;
            int totalPacks = 0;
            int totalInnerPacks = 0;
            String packsUnit = null;
            String innerPacksUnit = null;
            Map<String, Long> map = new HashMap<>();
            String toWeightUnit = Constants.WEIGHT_UNIT_KG;
            String toVolumeUnit = Constants.VOLUME_UNIT_M3;
            ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant())).get(0);
            if(!IsStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()))
                toWeightUnit = shipmentSettingsDetails.getWeightChargeableUnit();
            if(!IsStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()))
                toVolumeUnit = shipmentSettingsDetails.getVolumeChargeableUnit();
            if(packingList != null) {
                for (Packing packing: packingList) {
                    double winDef = convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), toWeightUnit).doubleValue();
                    double volDef = convertUnit(VOLUME, packing.getVolume(), packing.getVolumeUnit(), toVolumeUnit).doubleValue();
                    double netWtDif = convertUnit(MASS, packing.getNetWeight(), packing.getNetWeightUnit(), toWeightUnit).doubleValue();
                    totalWeight = totalWeight + winDef;
                    volumeWeight = volumeWeight + volDef;
                    netWeight = netWeight + netWtDif;
                    if(!IsStringNullOrEmpty(packing.getPacksType())) {
                        if(packsUnit == null)
                            packsUnit = packing.getPacksType();
                        else if(!packsUnit.equals(packing.getPacksType()))
                            packsUnit = MPK;
                        if(!map.containsKey(packing.getPacksType()))
                            map.put(packing.getPacksType(), 0L);
                    }
                    if(!IsStringNullOrEmpty(packing.getPacks())) {
                        int packs = Integer.parseInt(packing.getPacks());
                        totalPacks = totalPacks + packs;
                        if(!IsStringNullOrEmpty(packing.getPacksType())) {
                            map.put(packing.getPacksType(), map.get(packing.getPacksType()) + packs);
                        }
                    }
                    if(!IsStringNullOrEmpty(packing.getInnerPackageNumber())) {
                        int innerPacks = Integer.parseInt(packing.getInnerPackageNumber());
                        totalInnerPacks = totalInnerPacks + innerPacks;
                    }
                    if(!IsStringNullOrEmpty(packing.getInnerPackageType())) {
                        if(innerPacksUnit == null)
                            innerPacksUnit = packing.getInnerPackageType();
                        else if(!innerPacksUnit.equals(packing.getInnerPackageType()))
                            innerPacksUnit = MPK;
                    }
                }
            }
            volumetricWeight = volumeWeight * 166.667;
            chargeableWeight = Math.max(volumetricWeight, totalWeight);
            List<String> sortedKeys = new ArrayList<>(map.keySet());
            Collections.sort(sortedKeys);
            for (int i=0; i<sortedKeys.size(); i++) {
                Long value = map.get(sortedKeys.get(i));
                packsCount.append(value.toString()).append(" ").append(sortedKeys.get(i));
                if (i + 1 < sortedKeys.size())
                    packsCount.append(", ");
            }
            response.setTotalPacks(packsCount.toString());
            response.setTotalPacksWeight(String.format(Constants.STRING_FORMAT, totalWeight, toWeightUnit));
            response.setTotalPacksVolume(String.format(Constants.STRING_FORMAT, volumeWeight, toVolumeUnit));
            response.setPacksVolumetricWeight(String.format(Constants.STRING_FORMAT, volumetricWeight, toWeightUnit));

            dto.setWeight(new BigDecimal(totalWeight));
            dto.setWeightUnit(toWeightUnit);
            dto.setNetWeight(new BigDecimal(netWeight));
            dto.setNetWeightUnit(toWeightUnit);
            dto.setVolume(new BigDecimal(volumeWeight));
            dto.setVolumeUnit(toVolumeUnit);
            dto.setNoOfPacks(String.valueOf(totalPacks));
            dto.setPacksUnit(packsUnit);
            dto.setInnerPacks(totalInnerPacks);
            dto.setInnerPackUnit(innerPacksUnit);

            String packChargeableWeightUnit = toWeightUnit;
            if (!IsStringNullOrEmpty(transportMode) && transportMode.equals(Constants.TRANSPORT_MODE_AIR))
                chargeableWeight = roundOffAirShipment(chargeableWeight);
            if (!IsStringNullOrEmpty(transportMode) && transportMode.equals(Constants.TRANSPORT_MODE_SEA) &&
                    !IsStringNullOrEmpty(containerCategory) && containerCategory.equals(Constants.SHIPMENT_TYPE_LCL)) {
                double volInM3 = convertUnit(VOLUME, new BigDecimal(volumeWeight), toVolumeUnit, Constants.VOLUME_UNIT_M3).doubleValue();
                double wtInKg = convertUnit(Constants.MASS, new BigDecimal(totalWeight), toWeightUnit, Constants.WEIGHT_UNIT_KG).doubleValue();
                chargeableWeight = Math.max(wtInKg / 1000, volInM3);
                packChargeableWeightUnit = Constants.VOLUME_UNIT_M3;
            }
            chargeableWeight = BigDecimal.valueOf(chargeableWeight).setScale(2, RoundingMode.HALF_UP).doubleValue();
            response.setPacksChargeableWeight(String.format(Constants.STRING_FORMAT, chargeableWeight, packChargeableWeightUnit));
            return response;
        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    public VolumeWeightChargeable calculateVolumetricWeightForAir(BigDecimal volume, BigDecimal weight, String transportMode, String weightUnit, String volumeUnit) throws RunnerException {
        VolumeWeightChargeable vwOb = new VolumeWeightChargeable();
        BigDecimal wtInKG = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
        BigDecimal vlInM3 = new BigDecimal(convertUnit(VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());
        BigDecimal factor = new BigDecimal("166.667");
        if (Objects.equals(transportMode, TRANSPORT_MODE_ROA)) {
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
        return vwOb;
    }

    public ResponseEntity<IRunnerResponse> calculateVolumetricWeightForAirAndChargeable(CommonRequestModel model) throws RunnerException {
        AutoCalculatePackingRequest request = (AutoCalculatePackingRequest) model.getData();
        AutoCalculatePackingResponse response = new AutoCalculatePackingResponse();
        if (!StringUtils.isEmpty(request.getTransportMode()) && !request.getTransportMode().equals(TRANSPORT_MODE_AIR)) {
            return ResponseHelper.buildSuccessResponse(response);
        }
        BigDecimal volume = request.getVolume();
        BigDecimal weight = request.getWeight();
        String transportMode = request.getTransportMode();
        String weightUnit = request.getWeightUnit();
        String volumeUnit = request.getVolumeUnit();
        if (volume == null || weight == null) {
            return ResponseHelper.buildFailedResponse("Volume or weight is 0");
        }
        var obj = calculateVolumetricWeightForAir(volume, weight, transportMode, weightUnit, volumeUnit);
        calculateChargeableForAir(response, request);
        response.setVolumeWeight(obj.getVolumeWeight());
        response.setVolumeWeightUnit(obj.getVolumeWeightUnit());
        return ResponseHelper.buildSuccessResponse(response);
    }

    public void calculateVolume(String widthUnit, String heightUnit, String lengthUnit, AutoCalculatePackingResponse pack, AutoCalculatePackingRequest request) throws RunnerException {

        if (request.getWidthUnit() == null || request.getLengthUnit() == null || request.getHeightUnit() == null) {
            return;
        }
        if (!lengthUnit.equals(heightUnit) || !heightUnit.equals(widthUnit))
            return;

        String quantity = request.getPacks();
        if (StringUtility.isEmpty(request.getPacks()) ||request.getLength() == null || request.getWidth() == null || request.getHeight() == null)
            return;
        Double len = request.getLength().doubleValue();
        Double width = request.getWidth().doubleValue();
        Double height = request.getHeight().doubleValue();
        String dimUnit = lengthUnit;

        Double vol = null;

        if (!StringUtils.isEmpty(quantity) && !Double.isNaN(Double.parseDouble(quantity)) &&
                Double.parseDouble(quantity) >= 0 && len != null && width != null && height != null &&
                !StringUtils.isEmpty(dimUnit) && !StringUtils.isEmpty(widthUnit) && !StringUtils.isEmpty(heightUnit) &&
                !StringUtils.isEmpty(lengthUnit) && widthUnit.equals(heightUnit) && heightUnit.equals(lengthUnit) &&
                (dimUnit.equals("CM") || dimUnit.equals("FT") || dimUnit.equals("IN") || dimUnit.equals("M") || dimUnit.equals("MM"))) {
            if (dimUnit.equals("CM")) {
                vol = (Double.parseDouble(quantity) * len * width * height) / 1000000;
            } else if (dimUnit.equals("FT")) {
                vol = ((Double.parseDouble(quantity) * len * width * height) / 35.3147);
            } else if (dimUnit.equals("IN")) {
                vol = ((Double.parseDouble(quantity) * len * width * height) / 61024);
            } else if (dimUnit.equals("M")) {
                vol = ((Double.parseDouble(quantity) * len * width * height));
            } else if (dimUnit.equals("MM")) {
                vol = ((Double.parseDouble(quantity) * len * width * height) / 1000000000);
            }
            pack.setVolumeUnit("M3");
            pack.setVolume(BigDecimal.valueOf(vol));
            request.setVolumeUnit("M3");
            request.setVolume(BigDecimal.valueOf(vol));
            if (vol > 0.0 && request.getTransportMode() != null && request.getTransportMode().equals(TRANSPORT_MODE_AIR)) {
                var obj = this.calculateVolumetricWeightForAir(BigDecimal.valueOf(vol), pack.getWeight(), request.getTransportMode(), request.getWeightUnit(), request.getVolumeUnit());
                calculateChargeableForAir(pack, request);
                pack.setVolumeWeight(obj.getVolumeWeight());
                pack.setVolumeWeightUnit(obj.getVolumeWeightUnit());
            }
        } else if (!(len == null && width == null && height == null)) {
            pack.setVolume(null);
        }

    }


    protected void checkVolumeUnit(String volumeUnit,
                                   String widthUnit,
                                   String lengthUnit,
                                   String heightUnit,
                                   AutoCalculatePackingRequest request,
                                   AutoCalculatePackingResponse response) throws RunnerException {
        if (!StringUtils.isEmpty(volumeUnit) && volumeUnit.equals("M3")) {
            calculateVolume(widthUnit, heightUnit, lengthUnit, response, request);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> autoCalculateVolumetricWeight(CommonRequestModel commonRequestModel) {

        AutoCalculatePackingRequest request = (AutoCalculatePackingRequest) commonRequestModel.getData();
        AutoCalculatePackingResponse response = new AutoCalculatePackingResponse();
        String responseMsg;
        try {
            String transportMode = request.getTransportMode();
            BigDecimal weight = request.getWeight();
            String weightUnit = request.getWeightUnit();
            BigDecimal volume = request.getVolume();
            String volumeUnit = request.getVolumeUnit();
            if(!request.isVolumeChange()) {
                checkVolumeUnit(VOLUME_UNIT_M3, request.getWidthUnit(), request.getLengthUnit(), request.getHeightUnit(), request, response);
            }
            if (weightUnit != null && volumeUnit != null) {
                VolumeWeightChargeable vwOb = consolidationService.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);
                response.setChargeable(vwOb.getChargeable());
                if (Objects.equals(transportMode, TRANSPORT_MODE_AIR)) {
                    BigDecimal charge = response.getChargeable();
                    BigDecimal half = new BigDecimal("0.50");
                    BigDecimal floor = charge.setScale(0, BigDecimal.ROUND_FLOOR);
                    if (charge.subtract(half).compareTo(floor) <= 0 && charge.compareTo(floor) != 0) {
                        charge = floor.add(half);
                    } else {
                        charge = charge.setScale(0, BigDecimal.ROUND_CEILING);
                    }
                    response.setChargeable(charge);
                }
                if (transportMode.equals(Constants.TRANSPORT_MODE_SEA) &&
                        (request.getContainerCategory() != null && request.getContainerCategory().equals(Constants.SHIPMENT_TYPE_LCL))) {
                    volume = new BigDecimal(convertUnit(VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());
                    weight = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
                    response.setChargeable(weight.divide(new BigDecimal("1000")).max(volume));
                    vwOb = consolidationService.calculateVolumeWeight(transportMode, Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, weight, volume);
                }
                response.setVolumeWeight(vwOb.getVolumeWeight());
                response.setVolumeWeightUnit(vwOb.getVolumeWeightUnit());
                response.setChargeableUnit(vwOb.getChargeableUnit());
            }
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> autoCalculateChargable(CommonRequestModel commonRequestModel) throws RunnerException {
        AutoCalculatePackingRequest request = (AutoCalculatePackingRequest) commonRequestModel.getData();
        AutoCalculatePackingResponse response = new AutoCalculatePackingResponse();
        calculateChargeable(request, response);
        return ResponseHelper.buildSuccessResponse(response);
    }

    private void calculateChargeable(AutoCalculatePackingRequest request, AutoCalculatePackingResponse response) throws RunnerException {
        if (StringUtility.isNotEmpty(request.getTransportMode())) {
            if (!request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)){
                String wunit = request.getWeightUnit();
                String vunit = request.getVolumeUnit();
                var vol = request.getVolume();
                var weight = request.getWeight();

                if (wunit != null && !wunit.isEmpty() && vunit != null && !vunit.isEmpty()) {
                    VolumeWeightChargeable vwobj = consolidationService.calculateVolumeWeight(request.getTransportMode(), wunit, vunit, weight, vol);

                    response.setVolumeWeight(vwobj.getVolumeWeight());
                    response.setVolumeWeightUnit(vwobj.getVolumeWeightUnit());

                    if (request.getTransportMode() != null && request.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                        if (request.getContainerCategory() != null && request.getContainerCategory().equals(SHIPMENT_TYPE_LCL)) {
                            calculateChargeableForSEA_LCL(response, request);
                        }
                    }
                }
            } else {
                calculateChargeableForAir(response, request);
            }
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> autoCalculateVolume(CommonRequestModel commonRequestModel) throws RunnerException {
        AutoCalculatePackingRequest request = (AutoCalculatePackingRequest) commonRequestModel.getData();
        AutoCalculatePackingResponse response = new AutoCalculatePackingResponse();
        if(!request.isVolumeChange()) {
            calculateVolume(request.getWidthUnit(), request.getHeightUnit(), request.getLengthUnit(), response, request);
        }
        calculateChargeable(request, response);
        return ResponseHelper.buildSuccessResponse(response);
    }

    public void calculateChargeableForAir(AutoCalculatePackingResponse response, AutoCalculatePackingRequest request) {
        if (request.getWeight() == null) {
            return;
        }

        if (request.getWeightUnit().equals("KG")) {
            var totalWeight = request.getWeight();
            var totalVolume = (request.getVolume() != null && Objects.equals(request.getVolumeUnit(), VOLUME_UNIT_M3)) ? request.getVolume() : BigDecimal.ZERO;
            response.setChargeableUnit("KG");
            response.setChargeable(calculateChargeableWeight(response, request, totalVolume, totalWeight));
        }
    }

    private BigDecimal calculateChargeableWeight(AutoCalculatePackingResponse response, AutoCalculatePackingRequest request, BigDecimal totalVolume, BigDecimal totalWeight) {
        var chargeableWeight = totalWeight;
        var factorForVW = BigDecimal.valueOf(166.667);
        var totalVolumeInM3 = totalVolume;
        var volumetricWeight = totalVolumeInM3.multiply(factorForVW);

        if (chargeableWeight.doubleValue() < volumetricWeight.doubleValue()) {
            chargeableWeight = volumetricWeight;
        }
        return chargeableWeight;
    }

    public void calculateChargeableForSEA_LCL(AutoCalculatePackingResponse response, AutoCalculatePackingRequest request) throws RunnerException {
        var wunit = request.getWeightUnit();
        var vunit = request.getVolumeUnit();
        var vol = request.getVolume();
        var weight = request.getWeight();
        var vol_in_m3 = BigDecimal.valueOf(convertUnit(VOLUME, vol, vunit, VOLUME_UNIT_M3).doubleValue());
        var weight_in_kg = BigDecimal.valueOf(convertUnit(MASS, weight, wunit, WEIGHT_UNIT_KG).doubleValue());
        response.setChargeable(BigDecimal.valueOf(Math.max(weight_in_kg.doubleValue() / 1000.0, vol_in_m3.doubleValue())));
        response.setChargeableUnit(VOLUME_UNIT_M3);
    }

    private double roundOffAirShipment(double charge) {
        if (charge - 0.50 <= Math.floor(charge) && charge != Math.floor(charge)) {
            charge = Math.floor(charge) + 0.5;
        } else {
            charge = Math.ceil(charge);
        }
        return charge;
    }

    @Override
    public ResponseEntity<IRunnerResponse> listPacksToDetach(CommonRequestModel commonRequestModel) throws RunnerException {
        DetachPacksListDto request = (DetachPacksListDto) commonRequestModel.getData();
        if(request == null || request.getContainerId() == null || request.getShipmentId() == null) {
            throw new ValidationException("Either shipmentId or containerId is incorrect!");
        }
        ListCommonRequest listCommonRequest = andCriteria("containerId", request.getContainerId(), "=", null);
        listCommonRequest  = andCriteria("shipmentId", request.getShipmentId(), "=", listCommonRequest);
        if(request.getPageNo() != null)
            listCommonRequest.setPageNo(request.getPageNo());
        if(request.getPageSize() != null)
            listCommonRequest.setPageSize(request.getPageSize());
        return list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @Override
    public ResponseEntity<IRunnerResponse> V1PackingCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException {
        PackingRequestV2 packingRequestV2 = (PackingRequestV2) commonRequestModel.getData();
        try {
            if (checkForSync && !Objects.isNull(syncConfig.IS_REVERSE_SYNC_ACTIVE) && !syncConfig.IS_REVERSE_SYNC_ACTIVE) {
                return syncQueueService.saveSyncRequest(SyncingConstants.PACKAGES, StringUtility.convertToString(packingRequestV2.getGuid()), packingRequestV2);
            }
            Optional<Packing> existingPacking = packingDao.findByGuid(packingRequestV2.getGuid());
            Packing packing = modelMapper.map(packingRequestV2, Packing.class);
            if (existingPacking != null && existingPacking.isPresent()) {
                packing.setId(existingPacking.get().getId());
                packing.setConsolidationId(existingPacking.get().getConsolidationId());
                packing.setShipmentId(existingPacking.get().getShipmentId());
            } else {
                if (packingRequestV2.getShipmentGuid() != null) {
                    Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(packingRequestV2.getShipmentGuid());
                    if (shipmentDetails.isPresent())
                        packing.setShipmentId(shipmentDetails.get().getId());
                }
                if (packingRequestV2.getConsolidationGuid() != null) {
                    Optional<ConsolidationDetails> consolidationDetails = consolidationDao.findByGuid(packingRequestV2.getConsolidationGuid());
                    if (consolidationDetails.isPresent())
                        packing.setConsolidationId(consolidationDetails.get().getId());
                }
            }
            packing = packingDao.save(packing);
            PackingResponse response = objectMapper.convertValue(packing, PackingResponse.class);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception ex) {
            String responseMsg = ex.getMessage() != null ? ex.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, ex);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(ex);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> V1BulkPackingCreateAndUpdate(CommonRequestModel commonRequestModel) {
        BulkPackingRequestV2 bulkContainerRequest = (BulkPackingRequestV2) commonRequestModel.getData();
        try {
            List<ResponseEntity<?>> responses = new ArrayList<>();
            for (PackingRequestV2 containerRequest : bulkContainerRequest.getBulkPacking())
                responses.add(this.V1PackingCreateAndUpdate(CommonRequestModel.builder()
                        .data(containerRequest)
                        .build(), true));
            return ResponseHelper.buildSuccessResponse(responses);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
    }

    private static Containers addWeightVolume(Packing request, Containers newContainer) throws RunnerException {
        if(newContainer != null && request != null) {
            if(IsStringNullOrEmpty(newContainer.getAchievedWeightUnit()))
                newContainer.setAchievedWeightUnit(newContainer.getAllocatedWeightUnit());
            if(IsStringNullOrEmpty(newContainer.getAchievedVolumeUnit()))
                newContainer.setAchievedVolumeUnit(newContainer.getAllocatedVolumeUnit());
            BigDecimal finalWeight = new BigDecimal(convertUnit(Constants.MASS, request.getWeight(), request.getWeightUnit(), newContainer.getAchievedWeightUnit()).toString());
            BigDecimal finalVolume = new BigDecimal(convertUnit(VOLUME, request.getVolume(), request.getVolumeUnit(), newContainer.getAchievedVolumeUnit()).toString());
            if(newContainer.getAchievedWeight() != null)
                finalWeight = finalWeight.add(newContainer.getAchievedWeight());
            if(newContainer.getAchievedVolume() != null)
                finalVolume = finalVolume.add(newContainer.getAchievedVolume());
            newContainer.setAchievedWeight(finalWeight);
            newContainer.setAchievedVolume(finalVolume);
        }
        return newContainer;
    }

    private static Containers subtractWeightVolume(Packing request, Containers oldContainer) throws RunnerException {
        if(oldContainer != null && request != null) {
            if(IsStringNullOrEmpty(oldContainer.getAchievedWeightUnit()))
                oldContainer.setAchievedWeightUnit(oldContainer.getAllocatedWeightUnit());
            if(IsStringNullOrEmpty(oldContainer.getAchievedVolumeUnit()))
                oldContainer.setAchievedVolumeUnit(oldContainer.getAllocatedVolumeUnit());
            BigDecimal finalWeight = new BigDecimal(convertUnit(Constants.MASS, request.getWeight(), request.getWeightUnit(), oldContainer.getAchievedWeightUnit()).toString());
            BigDecimal finalVolume = new BigDecimal(convertUnit(VOLUME, request.getVolume(), request.getVolumeUnit(), oldContainer.getAchievedVolumeUnit()).toString());
            if(oldContainer.getAchievedWeight() != null) {
                finalWeight = oldContainer.getAchievedWeight().subtract(finalWeight);
                oldContainer.setAchievedWeight(finalWeight);
            }
            if(oldContainer.getAchievedVolume() != null) {
                finalVolume = oldContainer.getAchievedVolume().subtract(finalVolume);
                oldContainer.setAchievedVolume(finalVolume);
            }
        }
        return oldContainer;
    }

    private IRunnerResponse convertEntityToDto(Packing packing) {
        return jsonHelper.convertValue(packing, PackingResponse.class);
    }

    private IRunnerResponse convertEntityToDto(Containers packing) {
        return jsonHelper.convertValue(packing, ContainerResponse.class);
    }

    private Packing convertRequestToEntity(PackingRequest request) {
        return jsonHelper.convertValue(request, Packing.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Packing> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(packing -> {
            responseList.add(convertEntityToDto(packing));
        });
        return responseList;
    }

}
