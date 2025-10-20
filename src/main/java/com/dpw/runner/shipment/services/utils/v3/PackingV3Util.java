package com.dpw.runner.shipment.services.utils.v3;

import static com.dpw.runner.shipment.services.commons.constants.Constants.CONTENT_TYPE_FOR_EXCEL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.MASS;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_AIR;
import static com.dpw.runner.shipment.services.commons.constants.Constants.VOLUME;
import static com.dpw.runner.shipment.services.commons.constants.Constants.YYYY_MM_DD_HH_MM_SS_FORMAT;
import static com.dpw.runner.shipment.services.entity.enums.DateBehaviorType.ACTUAL;
import static com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus.SAILED;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackUtilizationV3Request;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.dto.request.AllocationsRequest;
import com.dpw.runner.shipment.services.dto.request.PackingExcelModel;
import com.dpw.runner.shipment.services.dto.request.ShipmentOrderAttachDetachRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentOrderV3Request;
import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.dto.response.AllocationsResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.v3.request.OrderLineV3Response;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.entity.AchievedQuantities;
import com.dpw.runner.shipment.services.entity.Allocations;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentOrder;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentOrderService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.ExcelCell;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.nimbusds.jose.util.Pair;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Component
public class PackingV3Util {

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IShipmentOrderService shipmentOrderService;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private MasterDataKeyUtils masterDataKeyUtils;

    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Autowired
    @Lazy
    private IConsolidationV3Service consolidationV3Service;

    @Autowired
    @Lazy
    private IShipmentServiceV3 shipmentService;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IPackingService packingService;

    private List<String> columnsSequenceForExcelDownloadForCargo = List.of(
            "guid", "shipmentNumber", "packs", "packsType", "innerPackageNumber", "innerPackageType", "origin", "packingOrder",
            "length", "lengthUnit", "width", "widthUnit", "height", "heightUnit", "weight", "weightUnit", "volume", "volumeUnit",
            "netWeight", "netWeightUnit", "chargeable", "chargeableUnit", "marksnNums", "countryCode", "goodsDescription",
            "referenceNumber", "inspections", "DGClass", "DGSubstanceId", "flashPoint", "UNDGContact", "isTemperatureControlled",
            "minTemp", "minTempUnit", "maxTemp", "maxTempUnit", "commodity", "HSCode", "customsReleaseCode", "unNumberAir", "dgClassAir",
            "dgClassAirDescription"
    );

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
                    result = result.stream().filter(result::contains).toList();
                }
            }
            LocalDateTime currentTime = LocalDateTime.now();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(YYYY_MM_DD_HH_MM_SS_FORMAT);
            String timestamp = currentTime.format(formatter);
            String filenameWithTimestamp = "CargoDetails_" + timestamp + Constants.XLSX;

            try(XSSFWorkbook workbook = new XSSFWorkbook()) {
                XSSFSheet sheet = workbook.createSheet("CargoDetails");

                List<PackingExcelModel> modelList = commonUtils.convertToList(result, PackingExcelModel.class);
                convertModelToExcel(modelList, sheet);

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

    private void convertModelToExcel(List<PackingExcelModel> modelList, XSSFSheet sheet) throws IllegalAccessException {

        // Create header row using annotations for order
        Row headerRow = sheet.createRow(0);
        Field[] fields = PackingExcelModel.class.getDeclaredFields();

        Map<String, Field> fieldNameMap = Arrays.stream(fields).filter(f->f.isAnnotationPresent(ExcelCell.class)).collect(Collectors.toMap(Field::getName, c-> c));
        columnsToIgnore(fieldNameMap);

        processOriginFieldNameMap(modelList, fieldNameMap);
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

    private void processOriginFieldNameMap(List<PackingExcelModel> modelList, Map<String, Field> fieldNameMap) {
        if(fieldNameMap.containsKey("origin")) {
            Set<String> unlocationsRefGuids = new HashSet<>();
            for (PackingExcelModel model : modelList) {
                if (!isStringNullOrEmpty(model.getOrigin()))
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
    }

    private void columnsToIgnore(Map<String, Field> fieldNameMap) {
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

    public void addAllMasterDataInSingleCallList(List<PackingResponse> packingListResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>();

            packingListResponse.forEach(pack -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(pack, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + pack.getId(), cacheMap)));

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            if (masterDataResponse == null) {
                packingListResponse.forEach(pack -> pack.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Packing.class.getSimpleName() + pack.getId()), CacheConstants.MASTER_LIST, cacheMap)));
            } else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse, cacheMap);
            }

            CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCallPacksList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), Packing.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public void addAllUnlocationInSingleCallList(List<PackingResponse> packingListResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>();

            packingListResponse.forEach(pack -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(pack, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + pack.getId(), cacheMap)));

            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);

            if (masterDataResponse == null) {
                packingListResponse.forEach(pack -> pack.setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Packing.class.getSimpleName() + pack.getId()), CacheConstants.UNLOCATIONS, cacheMap)));
            } else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS, masterDataResponse, cacheMap);
            }
            CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllUnlocationInSingleCallListPacksList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), Packing.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public void addAllCommodityTypesInSingleCallList(List<PackingResponse> packingListResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> commodityTypes = new HashSet<>();
            packingListResponse.forEach(pack -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(pack, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + pack.getId(), cacheMap)));

            Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY, commodityTypes, new EntityTransferCommodityType(), cacheMap);

            if (masterDataResponse == null) {
                packingListResponse.forEach(pack -> pack.setCommodityTypeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Packing.class.getSimpleName() + pack.getId()), CacheConstants.COMMODITY, cacheMap)));
            } else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COMMODITY, masterDataResponse, cacheMap);
            }
            CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCommodityTypesInSingleCallListPacksList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), Packing.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<Map<String, EntityTransferMasterLists>> addAllMasterDataInSingleCall(PackingResponse packingResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>(masterDataUtils.createInBulkMasterListRequest(packingResponse, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName(), cacheMap));

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList("ItemType", "ItemValue", "ItemDescription", "ValuenDesc", "Cascade"));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            if(masterDataResponse == null) {
                packingResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Packing.class.getSimpleName()), CacheConstants.MASTER_LIST, cacheMap));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(keyMasterDataMap);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), PackingV3Util.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<Map<String, EntityTransferUnLocations>> addAllUnlocationDataInSingleCall(PackingResponse packingResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>(masterDataUtils.createInBulkUnLocationsRequest(packingResponse, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName(), cacheMap));

            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);

            if(masterDataResponse == null) {
                packingResponse.setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Packing.class.getSimpleName()), CacheConstants.UNLOCATIONS, cacheMap));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(keyMasterDataMap);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllUnlocationDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), PackingV3Util.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<Map<String, EntityTransferCommodityType>> addAllCommodityTypesInSingleCall(PackingResponse packingResponse, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> commodityTypes = new HashSet<>(masterDataUtils.createInBulkCommodityTypeRequest(packingResponse, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName(), cacheMap));

            Map<String, EntityTransferCommodityType> keyMasterDataMap = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.COMMODITY, commodityTypes, new EntityTransferCommodityType(), cacheMap);

            if(masterDataResponse == null) {
                packingResponse.setCommodityTypeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Packing.class.getSimpleName()), CacheConstants.COMMODITY, cacheMap));
            }
            else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COMMODITY, masterDataResponse, cacheMap);
            }

            return CompletableFuture.completedFuture(keyMasterDataMap);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCommodityTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), PackingV3Util.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    public Long updateConsolidationIdInPackings(ShipmentDetails shipmentDetails, List<Packing> packings) {
        if(Constants.TRANSPORT_MODE_AIR.equals(shipmentDetails.getTransportMode())) {
            Long consolidationId = getConsolidationId(shipmentDetails.getId());
            for (Packing packing : packings) {
                packing.setConsolidationId(consolidationId);
            }
            return consolidationId;
        }
        return null;
    }

    public ShipmentDetails getShipmentById(ShipmentOrder shipmentOrder) {
        Long shipmentId = shipmentOrder.getShipmentId();
        return shipmentService.findById(shipmentId).orElseThrow(() -> new ValidationException("Shipment not found"));
    }

    public Long getConsolidationId(Long shipmentId) {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentId(shipmentId);
        if (!CommonUtils.listIsNullOrEmpty(consoleShipmentMappings)) {
            return consoleShipmentMappings.get(0).getConsolidationId();
        }
        return null;
    }

    public Long getShipmentId(Long consolidationId) {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationId);
        if (!CommonUtils.listIsNullOrEmpty(consoleShipmentMappings)) {
            return consoleShipmentMappings.get(0).getShipmentId();
        }
        return null;
    }

    public void processPackingRequests(List<Packing> packings, ShipmentDetails shipmentDetails) {
        boolean fullGated = true;
        boolean partialGated = false;
        boolean fullAssigned = true;
        boolean partialAssigned = false;
        LocalDateTime maxDate = null;
        for (Packing packing: packings) {
            if(packing.getCargoGateInDate() != null) {
                if(ACTUAL.equals(packing.getDateType()))
                    partialGated = true;
                else
                    fullGated = false;
                maxDate = getMaxDate(shipmentDetails, packing, maxDate);
            }
            else
                fullGated = false;
            if(packing.getContainerId() != null)
                partialAssigned = true;
            else
                fullAssigned = false;
        }
        setShipmentPackStatusOnAssigned(shipmentDetails, partialAssigned, fullAssigned, partialGated, fullGated);
    }

    private LocalDateTime getMaxDate(ShipmentDetails shipmentDetails, Packing packing, LocalDateTime maxDate) {
        if(maxDate == null || packing.getCargoGateInDate().isAfter(maxDate)) {
            shipmentDetails.setShipmentGateInDate(packing.getCargoGateInDate());
            shipmentDetails.setDateType(packing.getDateType());
            maxDate = packing.getCargoGateInDate();
        }
        return maxDate;
    }

    public void setShipmentPackStatusSailed(ShipmentDetails shipmentDetails) {
        if(shipmentDetails.getCarrierDetails() != null && shipmentDetails.getCarrierDetails().getAtd() != null)
            shipmentDetails.setShipmentPackStatus(SAILED);
    }

    private void setShipmentPackStatusOnAssigned(ShipmentDetails shipmentDetails, boolean partialAssigned, boolean fullAssigned, boolean partialGated, boolean fullGated) {
        if(partialAssigned)
            shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.PARTIALLY_ASSIGNED);
        if(fullAssigned)
            shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.ASSIGNED);
        if(partialGated)
            shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.PARTIAL_CARGO_GATE_IN);
        if(fullGated)
            shipmentDetails.setShipmentPackStatus(ShipmentPackStatus.CARGO_GATED_IN);
    }

    @Transactional
    public void savePackUtilisationCalculationInConsole(CalculatePackUtilizationV3Request calculatePackUtilizationRequest) {
        try {
            Optional<ConsolidationDetails> optional = consolidationV3Service.findById(calculatePackUtilizationRequest.getConsolidationId());
            if(optional.isPresent() && Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(optional.get().getTransportMode())) {
                var consolidation = optional.get();
                calculatePacksUtilisationForConsolidation(calculatePackUtilizationRequest);
                consolidationV3Service.save(consolidation, false);
            }
        }
        catch (Exception e) {
            log.error("Error saving pack utilisation in console : {}", e.getMessage());
        }
    }

    // For inter branch context (hub/coload both possible) we are relying on source functions
    public PackSummaryResponse calculatePacksUtilisationForConsolidation(CalculatePackUtilizationV3Request request) throws RunnerException {
        var consolidationId = request.getConsolidationId();
        var updatedConsolPacks = jsonHelper.convertValueToList(request.getPackingList(), Packing.class);
        List<Packing> shipmentPackingList = request.getShipmentPackingList();
        var allocated = jsonHelper.convertValue(request.getAllocationsRequest(), Allocations.class);
        var attachingShipments = request.getShipmentIdList();

        Optional<ConsolidationDetails> optionalConsol = consolidationV3Service.findById(consolidationId);
        ConsolidationDetails consol = null;
        PackSummaryResponse packSummaryResponse = null;
        AchievedQuantities achievedQuantities = null;
        String toWeightUnit = Optional.ofNullable(request.getAllocationsRequest()).map(AllocationsRequest::getWeightUnit).orElse(Constants.WEIGHT_UNIT_KG);
        String toVolumeUnit = Optional.ofNullable(request.getAllocationsRequest()).map(AllocationsRequest::getVolumeUnit).orElse(Constants.VOLUME_UNIT_M3);

        var packingList = new ArrayList<Packing>();

        if(optionalConsol.isEmpty()) {
            return null;
        }

        consol = optionalConsol.get();
        if(!Objects.isNull(allocated)) {
            consol.setAllocations(allocated);
        }
        achievedQuantities = Optional.ofNullable(consol.getAchievedQuantities()).orElse(new AchievedQuantities());
        achievedQuantities.setConsolidatedWeightUnit(toWeightUnit);
        achievedQuantities.setConsolidatedVolumeUnit(toVolumeUnit);

        List<Packing> consolPackingList = packingDao.findByConsolidationId(consolidationId);

        if(!CommonUtils.listIsNullOrEmpty(shipmentPackingList)) {
            var shipmentId = shipmentPackingList.get(0).getShipmentId();
            // Filter out the old shipment-linked packs from the consol packs stream
            packingList.addAll(consolPackingList.stream().filter(i -> !Objects.equals(i.getShipmentId(), shipmentId)).toList());
            // Add the current updated packs of the shipment
            packingList.addAll(shipmentPackingList);
        }
        else if (attachingShipments != null && !attachingShipments.isEmpty()) {
            Set<Long> packingIdSet = new HashSet<>();
            if(!Boolean.TRUE.equals(request.getIgnoreConsolidationPacks())) {
                packingList.addAll(consolPackingList);
                packingIdSet = consolPackingList.stream().map(Packing::getId).collect(Collectors.toSet());
            }
            Set<Long> finalPackingIdSet = packingIdSet;
            packingList.addAll(getShipmentPacks(attachingShipments).stream().filter(i -> !finalPackingIdSet.contains(i.getId())).toList());
        }
        else {
            // Default case of packs updated from consol
            packingList.addAll(Optional.ofNullable(updatedConsolPacks).orElse(Optional.ofNullable(consolPackingList).orElse(Collections.emptyList())));
        }

        log.info("calculating pack summary for aggregate of {} packs", packingList.size());
        packSummaryResponse = packingService.calculatePackSummary(packingList, TRANSPORT_MODE_AIR, null, new ShipmentMeasurementDetailsDto());
        log.info("Received weight: {} and volume:{} from packing summary response", packSummaryResponse.getAchievedWeight(), packSummaryResponse.getAchievedVolume());

        // only process the below calculation if the consolidation is air , exp , co-loading = true, allocated != null
        if(coLoadingConsolChecks(consol)) {
            var convertedWeight = BigDecimal.valueOf(convertUnit(MASS, packSummaryResponse.getAchievedWeight(), packSummaryResponse.getWeightUnit(), toWeightUnit).doubleValue());
            var convertedVolume = BigDecimal.valueOf(convertUnit(VOLUME, packSummaryResponse.getAchievedVolume(), packSummaryResponse.getVolumeUnit(), toVolumeUnit).doubleValue());
            achievedQuantities.setConsolidatedWeight(convertedWeight);
            achievedQuantities.setConsolidatedVolume(convertedVolume);
            consol.setAchievedQuantities(achievedQuantities);
            consol = commonUtils.calculateConsolUtilization(consol);
            packSummaryResponse.setConsolidationAchievedQuantities(jsonHelper.convertValue(consol.getAchievedQuantities(), AchievedQuantitiesResponse.class));
            packSummaryResponse.setAllocatedWeight(consol.getAllocations().getWeight());
            packSummaryResponse.setAllocatedVolume(consol.getAllocations().getVolume());
            packSummaryResponse.setAllocationsResponse(jsonHelper.convertValue(consol.getAllocations(), AllocationsResponse.class));
        }

        return packSummaryResponse;
    }

    private boolean coLoadingConsolChecks(ConsolidationDetails consolidation) {
        boolean flag = consolidation.getTransportMode().equalsIgnoreCase(TRANSPORT_MODE_AIR);
        if(!Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsMAWBColoadingEnabled()))
            flag = false;
        if(!(consolidation.getAllocations() != null && consolidation.getAllocations().getWeight() != null))
            flag = false;
        return flag;
    }

    private List<Packing> getShipmentPacks(List<Long> shipmentIds) {
        return packingDao.findByShipmentIdIn(shipmentIds);
    }

    public void setColoadingStation(ShipmentDetails shipmentDetails) {
        var tenantSettings = commonUtils.getCurrentTenantSettings();
        if (Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                && Boolean.TRUE.equals(tenantSettings.getIsMAWBColoadingEnabled())) {
            commonUtils.setInterBranchContextForColoadStation();
        }
    }

    public List<OrderLineV3Response> mapPackingV3RequestListToOrderLineList(List<PackingV3Request> packingRequests) {
        if (packingRequests == null || packingRequests.isEmpty()) {
            return Collections.emptyList();
        }
        return packingRequests.stream()
                .filter(Objects::nonNull)
                .map(this::mapPackingV3RequestToOrderLine)
                .toList();
    }

    public OrderLineV3Response mapPackingV3RequestToOrderLine(PackingV3Request packingRequest) {
        if (packingRequest == null) {
            return null;
        }
        return OrderLineV3Response.builder()
                .id(packingRequest.getId())
                .guid(packingRequest.getGuid())
                .commodityGroup(packingRequest.getCommodityGroup())
                .containerNumber(packingRequest.getContainerNumber())
                .containerId(packingRequest.getContainerId())
                .goodsDescription(packingRequest.getGoodsDescription())
                .HSCode(packingRequest.getHSCode())
                .length(packingRequest.getLength())
                .lengthUnit(packingRequest.getLengthUnit())
                .width(packingRequest.getWidth())
                .widthUnit(packingRequest.getWidthUnit())
                .height(packingRequest.getHeight())
                .heightUnit(packingRequest.getHeightUnit())
                .weight(packingRequest.getWeight())
                .weightUnit(packingRequest.getWeightUnit())
                .volume(packingRequest.getVolume())
                .volumeUnit(packingRequest.getVolumeUnit())
                .netWeight(packingRequest.getNetWeight())
                .netWeightUnit(packingRequest.getNetWeightUnit())
                .packs(packingRequest.getPacks())
                .packsType(packingRequest.getPacksType())
                .orderLineId(packingRequest.getOrderLineId())
                .lineNo(packingRequest.getLineNo())
                .subLineNo(packingRequest.getSubLineNo())
                .productCode(packingRequest.getProductCode())
                .shipmentOrderId(packingRequest.getShipmentOrderId())
                .orderGuid(packingRequest.getOrderGuid())
                .build();
    }

    public List<ShipmentOrderAttachDetachRequest.OrderDetails> mapToOrderDetailsList(List<ShipmentOrderV3Request> shipmentOrderV3List) {
        if (ObjectUtils.isEmpty(shipmentOrderV3List)) {
            return Collections.emptyList();
        }
        return shipmentOrderV3List.stream()
                .filter(Objects::nonNull)
                .map(this::mapToOrderDetails)
                .toList();
    }

    public ShipmentOrderAttachDetachRequest.OrderDetails mapToOrderDetails(ShipmentOrderV3Request request) {
        if (request == null) {
            return null;
        }
        List<OrderLineV3Response> orderPackings = (request.getOrderPackings() == null) ? Collections.emptyList() : request.getOrderPackings();
        return ShipmentOrderAttachDetachRequest.OrderDetails.builder()
                .orderNumber(request.getOrderNumber())
                .orderGuid(request.getOrderGuid())
                .shipmentId(request.getShipmentId())
                .orderDate(request.getOrderDate())
                .orderPackings(orderPackings)
                .build();
    }

    private Map<UUID, Long> fetchOrderGuidWithShipmentOrderIdMappings(List<OrderLineV3Response> orderLinesList) {
        log.info("Create orderGuid to shipmentOrder table's id mappings for request list with size {} with Request Id {}", orderLinesList.size(), LoggerHelper.getRequestIdFromMDC());
        List<UUID> orderGuidList = orderLinesList.stream()
                .filter(Objects::nonNull)
                .map(OrderLineV3Response::getOrderGuid)
                .toList();
        log.info("Fetch shipmentOrders from DB for orderGuid list with size {} with Request Id {}", orderGuidList.size(), LoggerHelper.getRequestIdFromMDC());
        List<ShipmentOrder> shipmentOrderList = shipmentOrderService.findByOrderGuidIn(orderGuidList);
        return shipmentOrderList.stream()
                .filter(shipmentOrder -> shipmentOrder.getOrderGuid() != null && shipmentOrder.getId() != null)
                .collect(Collectors.toMap(
                        ShipmentOrder::getOrderGuid,
                        ShipmentOrder::getId
                ));
    }

    public List<PackingV3Request> mapOrderLineListToPackingV3RequestList(List<OrderLineV3Response> orderLinesList) {
        if (orderLinesList == null || orderLinesList.isEmpty()) {
            log.info("Empty list order lines to map into packings with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return Collections.emptyList();
        }
        log.info("Map order lines into packings for request list with size {} with Request Id {}", orderLinesList.size(), LoggerHelper.getRequestIdFromMDC());
        Map<UUID, Long> mappings = fetchOrderGuidWithShipmentOrderIdMappings(orderLinesList);
        return orderLinesList.stream()
                .filter(Objects::nonNull)
                .map(orderLine -> mapOrderLineToPackingV3Request(orderLine, mappings))
                .toList();
    }

    public PackingV3Request mapOrderLineToPackingV3Request(OrderLineV3Response orderLineResponse, Map<UUID, Long> orderGuidWithShipmentOrderIdMapping) {
        if (orderLineResponse == null) {
            return null;
        }
        log.info("Map order line with guid {} into packing with Request Id {}", orderLineResponse.getOrderLineGuid(), LoggerHelper.getRequestIdFromMDC());
        Long shipmentOrderId = orderGuidWithShipmentOrderIdMapping.get(orderLineResponse.getOrderGuid());
        return PackingV3Request.builder()
                .commodityGroup(orderLineResponse.getCommodityGroup())
                .containerNumber(orderLineResponse.getContainerNumber())
                .containerId(orderLineResponse.getContainerId())
                .goodsDescription(orderLineResponse.getGoodsDescription())
                .HSCode(orderLineResponse.getHSCode())
                .commodity(orderLineResponse.getHSCode())
                .length(orderLineResponse.getLength())
                .lengthUnit(orderLineResponse.getLengthUnit())
                .width(orderLineResponse.getWidth())
                .widthUnit(orderLineResponse.getWidthUnit())
                .height(orderLineResponse.getHeight())
                .heightUnit(orderLineResponse.getHeightUnit())
                .weight(orderLineResponse.getWeight())
                .weightUnit(orderLineResponse.getWeightUnit())
                .volume(orderLineResponse.getVolume())
                .volumeUnit(orderLineResponse.getVolumeUnit())
                .netWeight(orderLineResponse.getNetWeight())
                .netWeightUnit(orderLineResponse.getNetWeightUnit())
                .packs(orderLineResponse.getPacks())
                .packsType(orderLineResponse.getPacksType())
                .orderLineId(orderLineResponse.getOrderLineId())
                .lineNo(orderLineResponse.getLineNo())
                .subLineNo(orderLineResponse.getSubLineNo())
                .productCode(orderLineResponse.getProductCode())
                .shipmentOrderId(shipmentOrderId)
                .orderGuid(orderLineResponse.getOrderGuid())
                .orderLineId(orderLineResponse.getOrderLineId())
                .orderLineGuid(orderLineResponse.getOrderLineGuid())
                .shipmentId(orderLineResponse.getShipmentId())
                .build();
    }
}
