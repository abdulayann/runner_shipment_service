package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackContainerNumberChangeRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.AutoCalculatePackingRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.response.AutoCalculatePackingResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.interfaces.ISyncQueueService;
import com.dpw.runner.shipment.services.syncing.Entity.BulkPackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingSync;
import com.dpw.runner.shipment.services.utils.CSVParsingUtil;
import com.dpw.runner.shipment.services.utils.ExcelUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
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
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
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

    private final CSVParsingUtil<Packing> parser = new CSVParsingUtil<>(Packing.class);

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
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
    public void uploadPacking(BulkUploadRequest request) throws Exception {
        List<Packing> packingList = parser.parseExcelFile(request.getFile());
        packingList.stream().forEach(packing -> {
            packing.setConsolidationId(request.getConsolidationId());
            packing.setShipmentId(request.getShipmentId());
        });
        packingDao.saveAll(packingList);
        packingSync.sync(packingList, request.getConsolidationId(), request.getShipmentId());
    }

    @Override
    public void downloadPacking(HttpServletResponse response, BulkDownloadRequest request) throws Exception {
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
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
            String timestamp = currentTime.format(formatter);
            String filenameWithTimestamp = "Packings_" + timestamp + ".xlsx";

            XSSFWorkbook workbook = new XSSFWorkbook();
            XSSFSheet sheet = workbook.createSheet("ContainerEvents");
            ExcelUtils utils = new ExcelUtils();
            utils.writeSimpleHeader(workbook, parser.generateCSVHeaderForPacking(), sheet);

            int rowNum = 1;
            for (Packing packing : result) {
                parser.addPackToSheet(packing, workbook, sheet, rowNum++);
            }

            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
            response.setHeader("Content-Disposition", "attachment; filename=" + filenameWithTimestamp);

            try (var outputStream = response.getOutputStream()) {
                workbook.write(outputStream);
            }

        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
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

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
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
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
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
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
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
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
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
    public ResponseEntity<?> calculateWeightVolumne(CommonRequestModel commonRequestModel) throws Exception {
        PackContainerNumberChangeRequest request = (PackContainerNumberChangeRequest) commonRequestModel.getData();
        List<IRunnerResponse> finalContainers = new ArrayList<>();
        ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant())).get(0);
        ShipmentDetails shipmentDetails = shipmentDao.findById(request.getPack().getShipmentId()).get();
        if(shipmentSettingsDetails.getMultipleShipmentEnabled() != null && shipmentSettingsDetails.getMultipleShipmentEnabled()
        && !shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL)
                && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA))) {

            Packing newPacking = jsonHelper.convertValue(request.getPack(), Packing.class);
            Packing oldPacking = newPacking;
            if(request.getPack().getId() != null) {
                oldPacking = packingDao.findById(request.getPack().getId()).get();
            }

            Containers oldContainer = null;
            if(request.getOldContainerId() != null)
                oldContainer = containersDao.findById(request.getOldContainerId()).get();
            Containers newContainer = null;
            if(request.getNewContainerId() != null)
                newContainer = containersDao.findById(request.getNewContainerId()).get();

            if(request.getOldContainerId() != null && request.getNewContainerId() != null && request.getOldContainerId().equals(request.getNewContainerId())) {
                subtractWeightVolume(oldPacking, newContainer);
                addWeightVolume(newPacking, newContainer);
            }
            else {
                oldContainer = subtractWeightVolume(oldPacking, oldContainer);
                newContainer = addWeightVolume(newPacking, newContainer);
                if(oldContainer != null)
                    finalContainers.add(jsonHelper.convertValue(oldContainer, ContainerResponse.class));
            }
            if(newContainer != null)
                finalContainers.add(jsonHelper.convertValue(newContainer, ContainerResponse.class));
        }
        return ResponseHelper.buildListSuccessResponse(finalContainers);
    }

    public PackSummaryResponse calculatePackSummary(List<Packing> packingList, String transportMode, String containerCategory) throws Exception {
        try {
            PackSummaryResponse response = new PackSummaryResponse();
            double totalWeight = 0;
            String packsCount = "";
            double volumeWeight = 0;
            double volumetricWeight = 0;
            double chargeableWeight = 0;
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
                    totalWeight = totalWeight + winDef;
                    volumeWeight = volumeWeight + volDef;
                    if(!IsStringNullOrEmpty(packing.getPacksType()) && !map.containsKey(packing.getPacksType()))
                        map.put(packing.getPacksType(), 0L);
                    if(!IsStringNullOrEmpty(packing.getPacks()) && !IsStringNullOrEmpty(packing.getPacksType()))
                        map.put(packing.getPacksType(), map.get(packing.getPacksType()) + Long.parseLong(packing.getPacks()));
                }
            }
            volumetricWeight = volumetricWeight * 166.667;
            chargeableWeight = Math.max(volumetricWeight, totalWeight);
            List<String> sortedKeys = new ArrayList<>(map.keySet());
            Collections.sort(sortedKeys);
            for (int i=0; i<sortedKeys.size(); i++) {
                Long value = map.get(sortedKeys.get(i));
                packsCount = packsCount + value.toString() + " " + sortedKeys.get(i);
                if (i + 1 < sortedKeys.size())
                    packsCount = packsCount + ", ";
            }
            response.setTotalPacks(packsCount);
            response.setTotalPacksWeight(totalWeight + " " + toWeightUnit);
            response.setTotalPacksVolume(volumeWeight + " " + toVolumeUnit);
            response.setPacksVolumetricWeight(volumetricWeight + " " + toWeightUnit);
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
            response.setPacksChargeableWeight(chargeableWeight + " " + packChargeableWeightUnit);
            return response;
        } catch (Exception e) {
            throw new Exception(e);
        }
    }

    public VolumeWeightChargeable calculateVolumetricWeightForAir(BigDecimal volume, BigDecimal weight, String transportMode, String weightUnit, String volumeUnit) throws Exception {
        VolumeWeightChargeable vwOb = new VolumeWeightChargeable();
        BigDecimal wtInKG = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
        BigDecimal vlInM3 = new BigDecimal(convertUnit(VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());
        BigDecimal factor = new BigDecimal(166.667);
        if (transportMode == Constants.TRANSPORT_MODE_ROA) {
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

    public void calculateVolume(String widthUnit, String heightUnit, String lengthUnit, AutoCalculatePackingResponse pack, AutoCalculatePackingRequest request) throws Exception {

        if (!lengthUnit.equals(heightUnit) || !heightUnit.equals(widthUnit))
            return;

        String quantity = request.getPacks();
        if (request.getLength() == null || request.getWidth() == null || request.getHeight() == null)
            throw new RunnerException("Length or height or width is null");
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
            if (vol != null && vol.doubleValue() > 0.0) {
                if (request.getTransportMode() != null && request.getTransportMode().equals(TRANSPORT_MODE_AIR)) {
                    this.calculateVolumetricWeightForAir(BigDecimal.valueOf(vol), pack.getWeight(), request.getTransportMode(), widthUnit, request.getVolumeUnit());
                }
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
                                   AutoCalculatePackingResponse response) throws Exception {
        if (!StringUtils.isEmpty(volumeUnit) && volumeUnit.equals("M3")) {
            calculateVolume(widthUnit, heightUnit, lengthUnit, response, request);
        }
    }

    @Override
    public ResponseEntity<?> autoCalculateVolumetricWeight(CommonRequestModel commonRequestModel) {

        AutoCalculatePackingRequest request = (AutoCalculatePackingRequest) commonRequestModel.getData();
        AutoCalculatePackingResponse response = new AutoCalculatePackingResponse();
        String responseMsg;
        try {
            String transportMode = request.getTransportMode();
            BigDecimal weight = request.getWeight();
            String weightUnit = request.getWeightUnit();
            BigDecimal volume = request.getVolume();
            String volumeUnit = request.getVolumeUnit();
            if (weightUnit != null && volumeUnit != null) {
                VolumeWeightChargeable vwOb = consolidationService.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);
                response.setChargeable(vwOb.getChargeable());
                if (transportMode == Constants.TRANSPORT_MODE_AIR) {
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
                response.setChargeableUnit(vwOb.getChargeableUnit());
                checkVolumeUnit(request.getVolumeUnit(), request.getWidthUnit(), request.getLengthUnit(), request.getHeightUnit(), request, response);
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
    public ResponseEntity<?> autoCalculateChargable(CommonRequestModel commonRequestModel) throws Exception {
        AutoCalculatePackingRequest request = (AutoCalculatePackingRequest) commonRequestModel.getData();
        AutoCalculatePackingResponse response = new AutoCalculatePackingResponse();
        calculateChargeable(request, response);
        return ResponseHelper.buildSuccessResponse(response);
    }

    private void calculateChargeable(AutoCalculatePackingRequest request, AutoCalculatePackingResponse response) throws Exception {
        if (StringUtility.isNotEmpty(request.getTransportMode()) && request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
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
        } else if (request.getTransportMode() != null && request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
            calculateChargeableForAir(response, request);
        }
    }

    @Override
    public ResponseEntity<?> autoCalculateVolume(CommonRequestModel commonRequestModel) throws Exception {
        AutoCalculatePackingRequest request = (AutoCalculatePackingRequest) commonRequestModel.getData();
        if (request.getVolumeUnit() == null || request.getWeightUnit() == null || request.getWidthUnit() == null || request.getLengthUnit() == null) {
            return ResponseEntity.badRequest().build();
        }
        AutoCalculatePackingResponse response = new AutoCalculatePackingResponse();
        calculateVolume(request.getWidthUnit(), request.getHeightUnit(), request.getLengthUnit(), response, request);
        calculateChargeable(request, response);
        return ResponseHelper.buildSuccessResponse(response);
    }

    public void calculateChargeableForAir(AutoCalculatePackingResponse response, AutoCalculatePackingRequest request) {
        if (request.getWeight() == null) {
            return;
        }

        if (request.getWeightUnit().equals("KG")) {
            var totalWeight = request.getWeight();
            var totalVolume = (request.getVolume() != null && request.getVolumeUnit() == VOLUME_UNIT_M3) ? request.getVolume() : BigDecimal.ZERO;
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

    public void calculateChargeableForSEA_LCL(AutoCalculatePackingResponse response, AutoCalculatePackingRequest request) throws Exception {
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
    public ResponseEntity<?> listPacksToDetach(CommonRequestModel commonRequestModel) throws Exception {
        Long containerId = commonRequestModel.getId();
        ListCommonRequest listCommonRequest = constructListCommonRequest("containerId", containerId, "=");
        return list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @Override
    public ResponseEntity<?> V1PackingCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws Exception {
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
    public ResponseEntity<?> V1BulkPackingCreateAndUpdate(CommonRequestModel commonRequestModel) {
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

    private static Containers addWeightVolume(Packing request, Containers newContainer) throws Exception {
        if(newContainer != null && request != null) {
            BigDecimal finalWeight = new BigDecimal(convertUnit(Constants.MASS, request.getWeight(), request.getWeightUnit(), newContainer.getAchievedWeightUnit()).toString());
            BigDecimal finalVolume = new BigDecimal(convertUnit(Constants.MASS, request.getVolume(), request.getVolumeUnit(), newContainer.getAchievedVolumeUnit()).toString());
            if(newContainer.getAchievedWeight() != null)
                finalWeight = finalWeight.add(newContainer.getAchievedWeight());
            if(newContainer.getAchievedVolume() != null)
                finalVolume = finalVolume.add(newContainer.getAchievedVolume());
            newContainer.setAchievedWeight(finalWeight);
            newContainer.setAchievedVolume(finalVolume);
        }
        return newContainer;
    }

    private static Containers subtractWeightVolume(Packing request, Containers oldContainer) throws Exception {
        if(oldContainer != null && request != null) {
            BigDecimal finalWeight = new BigDecimal(convertUnit(Constants.MASS, request.getWeight(), request.getWeightUnit(), oldContainer.getAchievedWeightUnit()).toString());
            BigDecimal finalVolume = new BigDecimal(convertUnit(Constants.MASS, request.getVolume(), request.getVolumeUnit(), oldContainer.getAchievedVolumeUnit()).toString());
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
