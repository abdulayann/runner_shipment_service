package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.Kafka.Dto.KafkaResponse;
import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerPartialListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.config.LocalTimeZoneHelper;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.TrackingService.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.patchRequest.CarrierPatchRequest;
import com.dpw.runner.shipment.services.dto.patchRequest.ConsolidationPatchRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.request.ConsoleBookingIdFilterRequest;
import com.dpw.runner.shipment.services.dto.v1.response.GuidsListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.mapper.CarrierDetailsMapper;
import com.dpw.runner.shipment.services.mapper.ConsolidationDetailsMapper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.CarrierResponse;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.AzureServiceBusTopic;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants.CONSOLIDATION_LIST_REQUEST_EMPTY_ERROR;
import static com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants.CONSOLIDATION_LIST_REQUEST_NULL_ERROR;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ConsolidationService implements IConsolidationService {

    @Autowired
    ExecutorService executorService;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private CSVParsingUtil<ConsolidationDetails> parser;

    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private INotesDao notesDao;

    @Autowired
    private IReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private ITruckDriverDetailsDao truckDriverDetailsDao;

    @Autowired
    private IRoutingsDao routingsDao;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private IContainerService containerService;

    @Autowired
    private IPackingService packingService;
    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private UserContext userContext;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IShipmentSync shipmentSync;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private ISBUtils sbUtils;

    @Autowired
    private ISBProperties isbProperties;

    @Autowired
    private AzureServiceBusTopic azureServiceBusTopic;

    @Autowired
    private IConsolidationSync consolidationSync;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private IAwbDao awbDao;

    @Autowired
    CacheManager cacheManager;

    @Autowired
    CustomKeyGenerator keyGenerator;

    @Autowired
    private MasterDataKeyUtils masterDataKeyUtils;

    @Autowired
    private ConsolidationDetailsMapper consolidationDetailsMapper;

    @Autowired
    private CarrierDetailsMapper carrierDetailsMapper;

    @Autowired
    private ProductIdentifierUtility productEngine;

    @Autowired
    private ITrackingServiceAdapter trackingServiceAdapter;

    @Autowired
    private KafkaProducer producer;

    @Autowired
    private GetNextNumberHelper getNextNumberHelper;

    @Autowired
    private IPackingsSync packingsADSync;
    @Autowired @Lazy
    private BookingIntegrationsUtility bookingIntegrationsUtility;

    @Value("${consolidationsKafka.queue}")
    private String senderQueue;
    private SecureRandom rnd = new SecureRandom();

    private List<String> TRANSPORT_MODES = Arrays.asList("SEA", "ROAD", "RAIL", "AIR");
    private List<String> SHIPMENT_TYPE = Arrays.asList("FCL", "LCL");
    private List<String> WEIGHT_UNIT = Arrays.asList("KGS", "G", "DT");
    private List<String> VOLUME_UNIT = Arrays.asList("M3", "L3", "CC");
    private List<String> SHIPPING_LINE = Arrays.asList("DPWC", "MARUSK", "APLU");
    private List<String> LOCATIONS = Arrays.asList("Jabel Ali", "Nava Shiva", "Shanghai", "Vancouver", "Seattle");
    private List<String> PARTY_TYPE = Arrays.asList("CLIENT", "CONSIGNER", "CONSIGNEE");
    private List<String> DIRECTIONS = Arrays.asList("IMP", "EXP");
    private List<String> SOURCE = Arrays.asList("API", "Runner", "Logistics");

    private Map<String, Object> ADDRESS = Map.ofEntries(
            Map.entry("AddressShortCode", "Default")
    );
    private Map<String, Object> ORG = Map.ofEntries(
            Map.entry("TenantName", "DP WORLD LOGISTICS CANADA INC")
    );
    private Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("id", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Long.class).build()),
//            Map.entry("type", RunnerEntityMapping.builder().tableName("parties").dataType(String.class).build()),
            Map.entry("cutoffDate", RunnerEntityMapping.builder().tableName("allocations").dataType(LocalDateTime.class).build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).build()),
//            Map.entry("bookingId", RunnerEntityMapping.builder().tableName("BookingCharges").dataType(Long.class).build()),
//            Map.entry("orderNumber", RunnerEntityMapping.builder().tableName("Jobs").dataType(String.class).build()),
            Map.entry(Constants.BOOKING_STATUS_FIELD, RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).fieldName(Constants.BOOKING_STATUS_FIELD).build()),
            Map.entry("orgCode", RunnerEntityMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("referenceNumber", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry(Constants.SHIPMENT_ID, RunnerEntityMapping.builder().tableName(Constants.SHIPMENTS_LIST).dataType(String.class).build()),
            Map.entry("consolidationNumber", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("consolidationType", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("transportMode", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("releaseType", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("deliveryMode", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("containerCategory", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("shipmentType", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("mawb", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("serviceLevel", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
//            Map.entry("bookingType", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("addressCode", RunnerEntityMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("shippingLine", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("vessel", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("voyage", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).isContainsText(true).build()),
            Map.entry("origin", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).build()),
            Map.entry("originPort", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).build()),
            Map.entry("destinationPort", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).build()),
            Map.entry("eta", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("etd", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("ata", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("containerNumber", RunnerEntityMapping.builder().tableName("containersList").dataType(String.class).build()),
            Map.entry("containerCode", RunnerEntityMapping.builder().tableName("containersList").dataType(String.class).build()),
            Map.entry("bookingCutoff", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("estimatedTerminalCutoff", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("hazardousBookingCutoff", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("isDomestic", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Boolean.class).build()),
            Map.entry("payment", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).build()),
            Map.entry("reeferCutoff", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("shipInstructionCutoff", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("terminalCutoff", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("verifiedGrossMassCutoff", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).build()),
            Map.entry("guid", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(UUID.class).fieldName("guid").build()),
            Map.entry("bol", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(String.class).isContainsText(true).fieldName("bol").build()),
            Map.entry("houseBill", RunnerEntityMapping.builder().tableName(Constants.SHIPMENTS_LIST).dataType(String.class).fieldName("houseBill").build()),
            Map.entry("voyageOrFlightNumber", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("voyageOrFlightNumber").build()),
            Map.entry("createdAt", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(LocalDateTime.class).fieldName("createdAt").build()),
            Map.entry("hazardous", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_DETAILS).dataType(Boolean.class).build())
            );

    @Override
    public ResponseEntity<IRunnerResponse> fetchConsolidations(CommonRequestModel commonRequestModel) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();

        Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
        Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
        return ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(consolidationDetailsPage.getContent()),
                consolidationDetailsPage.getTotalPages(),
                consolidationDetailsPage.getTotalElements());
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ConsolidationDetails> lst) {
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        List<IRunnerResponse> responseList = new ArrayList<>();
        List<ConsolidationListResponse> consolidationListResponses = new ArrayList<>();
        lst.forEach(consolidationDetails -> {
            var res = (modelMapper.map(consolidationDetails, ConsolidationListResponse.class));
            if(consolidationDetails.getBookingStatus() != null && Arrays.stream(CarrierBookingStatus.values()).map(CarrierBookingStatus::name).toList().contains(consolidationDetails.getBookingStatus()))
                res.setBookingStatus(CarrierBookingStatus.valueOf(consolidationDetails.getBookingStatus()).getDescription());
            updateHouseBillsShippingIds(consolidationDetails, res);
            containerCountUpdate(consolidationDetails, res, shipmentSettingsDetails.getIsShipmentLevelContainer() != null && shipmentSettingsDetails.getIsShipmentLevelContainer());
            consolidationListResponses.add(res);
        });
        consolidationListResponses.forEach(consolidationDetails -> {
            responseList.add(consolidationDetails);
        });
        try {
            var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID)), executorService);
            var containerTeuData = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setConsolidationContainerTeuData(lst, responseList)), executorService);
            CompletableFuture.allOf(locationDataFuture, containerTeuData).join();
        }
        catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
        }
        return responseList;
    }

    private void containerCountUpdate(ConsolidationDetails consolidationDetails, ConsolidationListResponse response, boolean isShipmentLevelContainer) {
        Long container20Count = 0L;
        Long container40Count = 0L;
        Long container20GPCount = 0L;
        Long container20RECount = 0L;
        Long container40GPCount = 0L;
        Long container40RECount = 0L;
        Long containerCount = 0L;
        Set<String> containerNumber = new HashSet<>();
        List<Containers> containersList = new ArrayList<>();
        containersList = consolidationDetails.getContainersList();
        if(isShipmentLevelContainer) {
            List<ShipmentDetails> shipmentDetails = consolidationDetails.getShipmentsList();
            if(shipmentDetails != null && shipmentDetails.size() > 0) {
                containersList = new ArrayList<>();
                for(ShipmentDetails shipmentDetails1 : shipmentDetails) {
                    if(shipmentDetails1.getContainersList() != null && shipmentDetails1.getContainersList().size() > 0) {
                        containersList.addAll(shipmentDetails1.getContainersList());
                    }
                }
            }
        } else {
            containersList = consolidationDetails.getContainersList();
        }
        if (containersList != null && containersList.size() > 0) {
            for (Containers container : containersList) {
                if(container.getContainerCode() != null) {
                    if (container.getContainerCode().contains(Constants.Cont20)) {
                        ++container20Count;
                    } else if (container.getContainerCode().contains(Constants.Cont40)) {
                        ++container40Count;
                    } else if (container.getContainerCode().equals(Constants.Cont20GP)) {
                        ++container20GPCount;
                    } else if (container.getContainerCode().equals(Constants.Cont20RE)) {
                        ++container20RECount;
                    } else if (container.getContainerCode().equals(Constants.Cont40GP)) {
                        ++container40GPCount;
                    } else if (container.getContainerCode().equals(Constants.Cont40RE)) {
                        ++container40RECount;
                    }
                }
                ++containerCount;
                if (StringUtility.isNotEmpty(container.getContainerNumber())) {
                    containerNumber.add(container.getContainerNumber());
                }
            }
        }
        response.setContainer20Count(container20Count);
        response.setContainer40Count(container40Count);
        response.setContainer20GPCount(container20GPCount);
        response.setContainer20RECount(container20RECount);
        response.setContainer40GPCount(container40GPCount);
        response.setContainer40RECount(container40RECount);
        response.setContainerCount(containerCount);
        response.setContainerNumbers(containerNumber);
    }

    private void updateHouseBillsShippingIds(ConsolidationDetails consol, ConsolidationListResponse consolidationRes) {
        var shipments = consol.getShipmentsList();
        List<String> shipmentIds = null;
        List<String> houseBills = null;
        if (shipments != null)
            shipmentIds = shipments.stream().map(shipment -> shipment.getShipmentId()).toList();
        if (shipmentIds != null)
            houseBills = shipments.stream().map(shipment -> shipment.getHouseBill()).filter(houseBill -> Objects.nonNull(houseBill)).toList();
        consolidationRes.setHouseBills(houseBills);
        consolidationRes.setShipmentIds(shipmentIds);
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {

        ConsolidationDetailsRequest request = (ConsolidationDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Consolidation Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        ConsolidationDetails consolidationDetails = jsonHelper.convertValue(request, ConsolidationDetails.class);
        try {
            ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
            consolidationDetails.setShipmentsList(null);

            beforeSave(consolidationDetails, null, true);

            getConsolidation(consolidationDetails);

            afterSave(consolidationDetails, null, request, true, shipmentSettingsDetails, false);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class));
    }

    @Transactional
    @Override
    public ResponseEntity<IRunnerResponse> createFromBooking(CommonRequestModel commonRequestModel) {

        ConsolidationDetailsRequest request = (ConsolidationDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Consolidation Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        ConsolidationDetails consolidationDetails = jsonHelper.convertValue(request, ConsolidationDetails.class);
        try {
            ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
            consolidationDetails.setShipmentsList(null);

            beforeSave(consolidationDetails, null, true);

            getConsolidation(consolidationDetails);

            afterSave(consolidationDetails, null, request, true, shipmentSettingsDetails, true);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class));
    }

    void getConsolidation(ConsolidationDetails consolidationDetails) throws RunnerException{
        generateConsolidationNumber(consolidationDetails);
        consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false);

        // audit logs
        try {
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(consolidationDetails)
                            .prevData(null)
                            .parent(ConsolidationDetails.class.getSimpleName())
                            .parentId(consolidationDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );
        }
        catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException | InvocationTargetException | NoSuchMethodException e) {
            log.error(e.getMessage());
            throw new RunnerException(e.getMessage());
        }

    }

    /**
     * Method that generates consol and bol number
     * @param consolidationDetails
     */
    @Override
    public void generateConsolidationNumber(ConsolidationDetails consolidationDetails) throws RunnerException {
        List<ShipmentSettingsDetails> shipmentSettingsList = shipmentSettingsDao.list();

        if(consolidationDetails.getConsolidationNumber() == null) {
            if(shipmentSettingsList.get(0) != null && shipmentSettingsList.get(0).getCustomisedSequence()) {
                String consoleNumber = getCustomizedConsolidationProcessNumber(consolidationDetails, shipmentSettingsList.get(0), ProductProcessTypes.ReferenceNumber);
                if(consoleNumber != null && !consoleNumber.isEmpty())
                    consolidationDetails.setConsolidationNumber(consoleNumber);
                if (consolidationDetails.getConsolidationNumber() == null || consolidationDetails.getConsolidationNumber().isEmpty())
                    consolidationDetails.setConsolidationNumber("CONS000" + getConsolidationSerialNumber());
                if (consolidationDetails.getReferenceNumber() == null || consolidationDetails.getReferenceNumber().isEmpty())
                    consolidationDetails.setReferenceNumber(consolidationDetails.getConsolidationNumber());
            }
            else {
                consolidationDetails.setConsolidationNumber("CONS000" + getConsolidationSerialNumber());
                if (consolidationDetails.getReferenceNumber() == null || consolidationDetails.getReferenceNumber().isEmpty())
                    consolidationDetails.setReferenceNumber(consolidationDetails.getConsolidationNumber());
            }
        }

        if(StringUtility.isEmpty(consolidationDetails.getBol())) {
            if(Objects.equals(ShipmentSettingsDetailsContext.getCurrentTenantSettings().getConsolidationLite(), false)) {
                String bol = getCustomizedConsolidationProcessNumber(consolidationDetails, shipmentSettingsList.get(0), ProductProcessTypes.BOLNumber);
                if (StringUtility.isEmpty(bol)) {
                    bol = generateCustomBolNumber();
                }
                if (StringUtility.isNotEmpty(bol)) {
                    consolidationDetails.setBol(bol);
                }
            }
        }
    }

    private String getConsolidationSerialNumber() {
        String maxId = v1Service.getMaxConsolidationId();
//        Long maxId = consolidationDetailsDao.findMaxId();
//        if(maxId == null)
//            maxId = 0L;
//        maxId += 1;
        return maxId;
    }

    private String getCustomizedConsolidationProcessNumber(ConsolidationDetails consolidationDetails, ShipmentSettingsDetails shipmentSettingsDetails, ProductProcessTypes productProcessTypes) throws RunnerException {
        List<TenantProducts> enabledTenantProducts = productEngine.populateEnabledTenantProducts(shipmentSettingsDetails);
        if (productProcessTypes == ProductProcessTypes.ReferenceNumber) {
            // to check the commmon sequence
            var sequenceNumber = productEngine.GetCommonSequenceNumber(consolidationDetails.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI);
            if (sequenceNumber != null && !sequenceNumber.isEmpty()) {
                return sequenceNumber;
            }
        }
        var identifiedProduct = productEngine.IdentifyProduct(consolidationDetails, enabledTenantProducts);
        if (identifiedProduct == null){
            return "";
        }
        var sequenceSettings = getNextNumberHelper.getProductSequence(identifiedProduct.getId(), productProcessTypes);
        if(sequenceSettings == null){
            return "";
        }
        String prefix = sequenceSettings.getPrefix() == null ? "" : sequenceSettings.getPrefix();
        var user = UserContext.getUser();
        return getNextNumberHelper.generateCustomSequence(sequenceSettings, prefix, user.getTenantId(), true, null, false);
    }

    public Optional<ConsolidationDetails> retrieveByIdOrGuid(ConsolidationDetailsRequest request) throws RunnerException {
        String responseMsg;

        if (request == null) {
            log.error("Request is empty for Consolidation update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        Optional<ConsolidationDetails> oldEntity = Optional.ofNullable(null);

        if(request.getId()!=null){
            long id = request.getId();
            oldEntity=consolidationDetailsDao.findById(id);
            if (!oldEntity.isPresent()) {
                log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_ERROR_WITH_REQUEST_ID, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

        }

        else if(request.getGuid()!=null){
            UUID guid = request.getGuid();
            oldEntity= consolidationDetailsDao.findByGuid(guid);
            if (!oldEntity.isPresent()) {
                log.debug("Consolidation Details is null for GUID {} with Request GUID {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

        }
        else{
            throw new RunnerException("Either Id or Guid is required");

        }
        return oldEntity;
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        try {
            ConsolidationDetailsRequest request = (ConsolidationDetailsRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Consolidation update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
//        if (request.getId() == null) {
//            log.error("Request Id is null for Consolidation update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
//        }
            // TODO- implement Validation logic

            Optional<ConsolidationDetails> oldEntity = retrieveByIdOrGuid(request);
            if (!oldEntity.isPresent()) {
                log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_ERROR_WITH_REQUEST_ID, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            ConsolidationDetails entity = jsonHelper.convertValue(request, ConsolidationDetails.class);
            entity.setId(oldEntity.get().getId());
            if(entity.getGuid() != null && !oldEntity.get().getGuid().equals(entity.getGuid())) {
                throw new RunnerException("Provided GUID doesn't match with the existing one !");
            }
            if (entity.getContainersList() == null)
                entity.setContainersList(oldEntity.get().getContainersList());
            beforeSave(entity, oldEntity.get(), false);
            entity = consolidationDetailsDao.update(entity, false);
            syncConsole(entity, false);
            pushShipmentDataToDependentService(entity, false);
            return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(entity, ConsolidationDetailsResponse.class));
        }
        catch (Exception e) {
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
    }

    private void syncConsole(ConsolidationDetails entity, boolean isDirectSync) {
        try {
            consolidationSync.sync(entity, StringUtility.convertToString(entity.getGuid()), isDirectSync);
        } catch (Exception e) {
            log.error("Error performing sync on consol entity, {}", e);
        }
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> attachShipments(Long consolidationId, List<Long> shipmentIds) throws RunnerException {

        if(consolidationId != null && shipmentIds!= null && shipmentIds.size() > 0) {
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, shipmentIds, "IN");
            Pair<Specification<ConsoleShipmentMapping>, Pageable> pair = fetchData(listCommonRequest, ConsoleShipmentMapping.class);
            Page<ConsoleShipmentMapping> oldConsoleShipmentMappings = consoleShipmentMappingDao.findAll(pair.getLeft(), pair.getRight());
            List<ConsoleShipmentMapping> consoleShipmentMappings = new ArrayList<>();
            if(oldConsoleShipmentMappings != null && !oldConsoleShipmentMappings.isEmpty()) {
                consoleShipmentMappings = oldConsoleShipmentMappings.getContent();
                for (ConsoleShipmentMapping consoleShipmentMapping : oldConsoleShipmentMappings.getContent()) {
                    if(!consoleShipmentMapping.getConsolidationId().equals(consolidationId))
                        return ResponseHelper.buildFailedResponse("Multiple consolidations are attached to the shipment, please verify.");
                }
            }
            List<Long> attachedShipmentIds = consoleShipmentMappingDao.assignShipments(consolidationId, shipmentIds, consoleShipmentMappings);
            for(Long shipId : attachedShipmentIds) {
                ShipmentDetails shipmentDetails = shipmentDao.findById(shipId).get();
                if(shipmentDetails.getContainersList() != null) {
                    List<Containers> containersList = shipmentDetails.getContainersList();
                    for(Containers container : containersList) {
                        container.setConsolidationId(consolidationId);
                    }
                    containersList = containerDao.saveAll(containersList);
                    containerService.afterSaveList(containersList, false);
                }
                if(shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getPackingList() != null) {
                    List<Packing> packingList = shipmentDetails.getPackingList();
                    for(Packing packing : packingList) {
                        packing.setConsolidationId(consolidationId);
                    }
                    packingList = packingDao.saveAll(packingList);
                }
            }
        }
        Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consolidationId);
        ConsolidationDetails consolidationDetails = null;
        if(consol.isPresent())
            consolidationDetails = consol.get();
        else
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        if(checkForNonDGConsoleAndAirDGFlag(consolidationDetails)) {
            ListCommonRequest listCommonRequest = constructListCommonRequest("id", shipmentIds, "IN");
            listCommonRequest = andCriteria("containsHazardous", true, "=", listCommonRequest);
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
            Page<ShipmentDetails> shipments = shipmentDao.findAll(pair.getLeft(), pair.getRight());
            if(shipments != null && !shipments.isEmpty()) {
                consolidationDetails.setHazardous(true);
                consolidationDetailsDao.update(consolidationDetails, false);
            }
        }
        updateLinkedShipmentData(consolidationDetails, null, true);
        try {
            consolidationSync.sync(consolidationDetails, StringUtility.convertToString(consolidationDetails.getGuid()), false);
        }
        catch (Exception e) {
            log.error("Error Syncing Consol");
        }
        return new ResponseEntity<>(HttpStatus.OK);
    }

    private boolean checkForNonDGConsoleAndAirDGFlag(ConsolidationDetails consolidationDetails) {
        if(!Boolean.TRUE.equals(ShipmentSettingsDetailsContext.getCurrentTenantSettings().getAirDGFlag()))
            return false;
        if(!Constants.TRANSPORT_MODE_AIR.equals(consolidationDetails.getTransportMode()))
            return false;
        return !Boolean.TRUE.equals(consolidationDetails.getHazardous());
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> detachShipments(Long consolidationId, List<Long> shipmentIds) {
        List<Packing> packingList = null;
        if(consolidationId != null && shipmentIds!= null && shipmentIds.size() > 0) {
            List<Long> removedShipmentIds = consoleShipmentMappingDao.detachShipments(consolidationId, shipmentIds);
            for(Long shipId : removedShipmentIds) {
                ShipmentDetails shipmentDetails = shipmentDao.findById(shipId).get();
                if(shipmentDetails.getContainersList() != null) {
                    List<Containers> containersList = shipmentDetails.getContainersList();
                    for(Containers container : containersList) {
                        shipmentsContainersMappingDao.detachShipments(container.getId(), List.of(shipId), false);
                    }
                    containersList = containerDao.saveAll(containersList);
                    containerService.afterSaveList(containersList, false);
                }
                if(shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getPackingList() != null) {
                    packingList = shipmentDetails.getPackingList();
                    for(Packing packing : packingList) {
                        packing.setConsolidationId(null);
                    }
                    packingList = packingDao.saveAll(packingList);
                }
            }
        }
        Optional<ConsolidationDetails> consol = consolidationDetailsDao.findById(consolidationId);
        if(consol.isPresent() && checkAttachDgAirShipments(consol.get())){
            consol.get().setHazardous(false);
            consolidationDetailsDao.save(consol.get(), false);
        }
        String transactionId = consol.get().getGuid().toString();
        if(packingList != null) {
            try {
                packingsADSync.sync(packingList, transactionId);
            } catch (Exception e) {
                log.error(SyncingConstants.ERROR_SYNCING_PACKS);
            }
        }
        try {
            consolidationSync.sync(consol.get(), transactionId, false);
        }
        catch (Exception e) {
            log.error("Error Syncing Consol");
        }

        return new ResponseEntity<>(HttpStatus.OK);
    }

    private boolean checkAttachDgAirShipments(ConsolidationDetails consolidationDetails){
        if(!Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR))
            return false;
        if(!Boolean.TRUE.equals(consolidationDetails.getHazardous()))
            return false;
        if(!Boolean.TRUE.equals(ShipmentSettingsDetailsContext.getCurrentTenantSettings().getAirDGFlag()))
            return false;
        if(consolidationDetails.getShipmentsList() == null || consolidationDetails.getShipmentsList().isEmpty())
            return true;
        Boolean isDgShipmentAttached = consolidationDetails.getShipmentsList().stream().anyMatch(ship -> Boolean.TRUE.equals(ship.getContainsHazardous()));
        return !isDgShipmentAttached;
    }

    @Transactional
    public ResponseEntity<HttpStatus> detachShipmentsReverseSync(Long consolidationId, List<Long> shipmentIds) {
        if(consolidationId != null && shipmentIds!= null && shipmentIds.size() > 0) {
            List<Long> removedShipmentIds = consoleShipmentMappingDao.detachShipments(consolidationId, shipmentIds);
            for(Long shipId : removedShipmentIds) {
                ShipmentDetails shipmentDetails = shipmentDao.findById(shipId).get();
                if(shipmentDetails.getContainersList() != null) {
                    List<Containers> containersList = shipmentDetails.getContainersList();
                    for(Containers container : containersList) {
                        shipmentsContainersMappingDao.detachShipments(container.getId(), List.of(shipId), true);
                    }
                    containerDao.saveAll(containersList);
                }
            }
        }

        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> partialUpdate(CommonRequestModel commonRequestModel, Boolean fromV1) throws RunnerException {
        ConsolidationPatchRequest consolidationDetailsRequest = (ConsolidationPatchRequest) commonRequestModel.getData();
        // Get old entity to be updated
        ConsolidationDetailsRequest req = new ConsolidationDetailsRequest();
        req.setId(consolidationDetailsRequest.getId());
        req.setGuid(consolidationDetailsRequest.getGuid());
        Optional<ConsolidationDetails> oldEntity = retrieveByIdOrGuid(req);
        try {
            ConsolidationDetails entity = oldEntity.get();
            long id = oldEntity.get().getId();
            consolidationDetailsMapper.update(consolidationDetailsRequest, entity);

            // Carrier Details Update
            CarrierPatchRequest carrierDetailRequest = consolidationDetailsRequest.getCarrierDetails();
            CarrierDetails updatedCarrierDetails = null;
            if (carrierDetailRequest != null) {
                updatedCarrierDetails = oldEntity.get().getCarrierDetails();
                carrierDetailsMapper.update(carrierDetailRequest, updatedCarrierDetails);
            }
            entity.setCarrierDetails(oldEntity.get().getCarrierDetails());

            beforeSave(entity, oldEntity.get(), false);
            entity = consolidationDetailsDao.update(entity, false);

            List<PackingRequest> packingRequestList = consolidationDetailsRequest.getPackingList();
            List<ContainerRequest> containerRequestList = consolidationDetailsRequest.getContainersList();
            List<EventsRequest> eventsRequestList = consolidationDetailsRequest.getEventsList();
            List<FileRepoRequest> fileRepoRequestList = consolidationDetailsRequest.getFileRepoList();
            List<JobRequest> jobRequestList = consolidationDetailsRequest.getJobsList();
            List<ReferenceNumbersRequest> referenceNumbersRequestList = consolidationDetailsRequest.getReferenceNumbersList();
            List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = consolidationDetailsRequest.getTruckDriverDetails();
            List<RoutingsRequest> routingsRequestList = consolidationDetailsRequest.getRoutingsList();
            List<PartiesRequest> consolidationAddressRequest = consolidationDetailsRequest.getConsolidationAddresses();

            if(containerRequestList != null) {
                List<Containers> updatedContainers = containerDao.updateEntityFromShipmentConsole(commonUtils.convertToEntityList(containerRequestList, Containers.class), id, (Long) null, true);
                entity.setContainersList(updatedContainers);
            }
            if (packingRequestList != null) {
                List<Packing> updatedPackings = packingDao.updateEntityFromConsole(commonUtils.convertToEntityList(packingRequestList, Packing.class), id);
                entity.setPackingList(updatedPackings);
            }
            if (eventsRequestList != null) {
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(eventsRequestList, Events.class), id, Constants.CONSOLIDATION);
                entity.setEventsList(updatedEvents);
            }
            if (referenceNumbersRequestList != null) {
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromConsole(commonUtils.convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id);
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (truckDriverDetailsRequestList != null) {
                List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromConsole(commonUtils.convertToEntityList(truckDriverDetailsRequestList, TruckDriverDetails.class), id);
//                entity.setTruckDriverDetails(updatedTruckDriverDetails);
            }
            if (routingsRequestList != null) {
                List<Routings> updatedRoutings = routingsDao.updateEntityFromConsole(commonUtils.convertToEntityList(routingsRequestList, Routings.class), id);
                entity.setRoutingsList(updatedRoutings);
            }
            if (consolidationAddressRequest != null) {
                List<Parties> updatedFileRepos = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(consolidationAddressRequest, Parties.class), id, Constants.CONSOLIDATION_ADDRESSES);
                entity.setConsolidationAddresses(updatedFileRepos);
            }
            if(fromV1 == null || !fromV1) {
                try {
                    consolidationSync.sync(entity, StringUtility.convertToString(entity.getGuid()), false);
                } catch (Exception e){
                    log.error("Error performing sync on consolidation entity, {}", e);
                }
            }
            pushShipmentDataToDependentService(entity, false);

            ConsolidationDetailsResponse response = jsonHelper.convertValue(entity, ConsolidationDetailsResponse.class);
            return ResponseHelper.buildSuccessResponse(response);

        } catch (Exception e){
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException {

        ConsolidationDetailsRequest consolidationDetailsRequest = (ConsolidationDetailsRequest) commonRequestModel.getData();
        // TODO- implement Validation logic

        Optional<ConsolidationDetails> oldEntity = retrieveByIdOrGuid(consolidationDetailsRequest);
        if (!oldEntity.isPresent()) {
            log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_FOR_GIVEN_ID_ERROR, consolidationDetailsRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        long id = oldEntity.get().getId();

        consolidationDetailsRequest.setShipmentsList(null);

        try {
            ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
            ConsolidationDetails entity = jsonHelper.convertValue(consolidationDetailsRequest, ConsolidationDetails.class);
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());

            beforeSave(entity, oldEntity.get(), false);

            entity = consolidationDetailsDao.update(entity, false);
            try {
                // audit logs
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .newData(entity)
                                .prevData(jsonHelper.readFromJson(oldEntityJsonString, ConsolidationDetails.class))
                                .parent(ConsolidationDetails.class.getSimpleName())
                                .parentId(entity.getId())
                                .operation(DBOperationType.UPDATE.name()).build()
                );
            }
            catch (Exception e) {
                log.error("Error writing audit service log", e);
            }

            afterSave(entity, oldEntity.get(), consolidationDetailsRequest, false, shipmentSettingsDetails, false);

            ConsolidationDetailsResponse response = jsonHelper.convertValue(entity, ConsolidationDetailsResponse.class);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RunnerException(e.getMessage());
        }
    }

    /**
     * update data to all the shipments linked to console
     * @param console
     * @param oldEntity
     */
    private void updateLinkedShipmentData(ConsolidationDetails console, ConsolidationDetails oldEntity, Boolean fromAttachShipment) throws RunnerException {
        V1TenantSettingsResponse tenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        if(Boolean.TRUE.equals(tenantSettingsResponse.getEnableAirMessaging()) && Objects.equals(console.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(console.getEfreightStatus(), Constants.EAW)){
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(console.getId());
            List<Long> shipmentIdList = consoleShipmentMappings.stream().map(i -> i.getShipmentId()).toList();
            ListCommonRequest listReq = constructListCommonRequest("id", shipmentIdList, "IN");
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listReq, ShipmentDetails.class);
            Page<ShipmentDetails> page = shipmentDao.findAll(pair.getLeft(), pair.getRight());
            List<ShipmentDetails> shipments = page.getContent();
            var shipmentlist = shipments.stream().filter(x-> Objects.equals(x.getAdditionalDetails().getEfreightStatus(), Constants.NON)).toList();
            if(shipmentlist != null && !shipmentlist.isEmpty()){
                throw new RunnerException("EFreight status can only be EAP or NON as one of the Shipment has EFreight status as NON");
            }
        }
        if(console != null && (oldEntity == null ||  !Objects.equals(console.getBol(),oldEntity.getBol()) ||
                !Objects.equals(console.getShipmentType(),oldEntity.getShipmentType()) ||
                (console.getCarrierDetails() != null && oldEntity.getCarrierDetails() != null &&
                (!Objects.equals(console.getCarrierDetails().getVoyage(),oldEntity.getCarrierDetails().getVoyage()) ||
                        !Objects.equals(console.getCarrierDetails().getVessel(),oldEntity.getCarrierDetails().getVessel()) ||
                        !Objects.equals(console.getCarrierDetails().getShippingLine(),oldEntity.getCarrierDetails().getShippingLine()) ||
                        !Objects.equals(console.getCarrierDetails().getAircraftType(),oldEntity.getCarrierDetails().getAircraftType())
                )))) {
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(console.getId());
            List<Long> shipmentIdList = consoleShipmentMappings.stream().map(i -> i.getShipmentId()).toList();
            ListCommonRequest listReq = constructListCommonRequest("id", shipmentIdList, "IN");
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listReq, ShipmentDetails.class);
            Page<ShipmentDetails> page = shipmentDao.findAll(pair.getLeft(), pair.getRight());

            List<ShipmentDetails> shipments = page.getContent();
            shipments.stream()
                    .map(i -> {
                        i.setConsolRef(console.getReferenceNumber());
                        i.setMasterBill(console.getBol());
                        i.setDirection(console.getShipmentType());
                        if (console.getCarrierDetails() != null) {
                            i.getCarrierDetails().setVoyage(console.getCarrierDetails().getVoyage());
                            i.getCarrierDetails().setVessel(console.getCarrierDetails().getVessel());
                            i.getCarrierDetails().setShippingLine(console.getCarrierDetails().getShippingLine());
                            i.getCarrierDetails().setAircraftType(console.getCarrierDetails().getAircraftType());
                            if(fromAttachShipment != null && fromAttachShipment){
                                i.getCarrierDetails().setEta(console.getCarrierDetails().getEta());
                                i.getCarrierDetails().setEtd(console.getCarrierDetails().getEtd());
                                i.getCarrierDetails().setFlightNumber(console.getCarrierDetails().getFlightNumber());
                            }
                        }

                        return i;
                    }).toList();

            shipmentDao.saveAll(shipments);
            if(fromAttachShipment != null && fromAttachShipment) {
                for (ShipmentDetails shipmentDetails : shipments) {
                    try {
                        shipmentSync.sync(shipmentDetails, null, null, StringUtility.convertToString(console.getGuid()), false);
                    } catch (Exception e) {
                        log.error("Error performing sync on shipment entity, {}", e);
                    }
                }
            }
        }
    }

    private ConsolidationDetails calculateConsolUtilization(ConsolidationDetails consolidationDetails) throws RunnerException {
        String responseMsg;
        try {
            if(consolidationDetails.getAllocations() == null)
                consolidationDetails.setAllocations(new Allocations());
            if(consolidationDetails.getAchievedQuantities() == null)
                consolidationDetails.setAchievedQuantities(new AchievedQuantities());
            if (consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit() != null && consolidationDetails.getAllocations().getWeightUnit() != null) {
                BigDecimal consolidatedWeight = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAchievedQuantities().getConsolidatedWeight(), consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
                BigDecimal weight = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAllocations().getWeight(), consolidationDetails.getAllocations().getWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
                if(Objects.equals(weight, BigDecimal.ZERO))
                    consolidationDetails.getAchievedQuantities().setWeightUtilization("0");
                else
                    consolidationDetails.getAchievedQuantities().setWeightUtilization( String.valueOf( (consolidatedWeight.divide(weight, 4, BigDecimal.ROUND_HALF_UP)).multiply(new BigDecimal(100)).doubleValue() ) );
            }
            if (consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit() != null && consolidationDetails.getAllocations().getVolumeUnit() != null) {
                BigDecimal consolidatedVolume = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAchievedQuantities().getConsolidatedVolume(), consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
                BigDecimal volume = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAllocations().getVolume(), consolidationDetails.getAllocations().getVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
                if(Objects.equals(volume, BigDecimal.ZERO))
                    consolidationDetails.getAchievedQuantities().setVolumeUtilization("0");
                else
                    consolidationDetails.getAchievedQuantities().setVolumeUtilization( String.valueOf( (consolidatedVolume.divide(volume, 4, BigDecimal.ROUND_HALF_UP)).multiply(new BigDecimal(100)).doubleValue() ) );
            }
            return consolidationDetails;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateUtilization(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ConsolidationDetails consolidationDetails = getConsoleForCalculations((ConsoleCalculationsRequest) commonRequestModel.getData());
            consolidationDetails = calculateConsolUtilization(consolidationDetails);
            ConsoleCalculationsResponse response = getConsoleCalculationsResponse(consolidationDetails);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private ConsolidationDetails getConsoleForCalculations(ConsoleCalculationsRequest request) {
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(request.getId()).get();
        consolidationDetails.setAllocations(jsonHelper.convertValue(request.getAllocations(), Allocations.class));
        consolidationDetails.setAchievedQuantities(jsonHelper.convertValue(request.getAchievedQuantities(), AchievedQuantities.class));
        consolidationDetails.setTransportMode(request.getTransportMode());
        consolidationDetails.setContainerCategory(request.getContainerCategory());
        return consolidationDetails;
    }

    private ConsoleCalculationsResponse getConsoleCalculationsResponse(ConsolidationDetails consolidationDetails) {
        ConsoleCalculationsResponse response = new ConsoleCalculationsResponse();
        response.setAllocations(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));
        return response;
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateAchieved_AllocatedForSameUnit(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ConsolidationDetails consolidationDetails = getConsoleForCalculations((ConsoleCalculationsRequest) commonRequestModel.getData());
            if(consolidationDetails.getAllocations() == null)
                consolidationDetails.setAllocations(new Allocations());
            if(consolidationDetails.getAchievedQuantities() == null)
                consolidationDetails.setAchievedQuantities(new AchievedQuantities());
            if (consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit() != consolidationDetails.getAllocations().getWeightUnit()) {
                BigDecimal val = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAchievedQuantities().getConsolidatedWeight(), consolidationDetails.getAchievedQuantities().getConsolidatedWeightUnit(), consolidationDetails.getAllocations().getWeightUnit()).toString());
                consolidationDetails.getAchievedQuantities().setConsolidatedWeight(val);
                consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(consolidationDetails.getAllocations().getWeightUnit());
            }
            if (consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit() != consolidationDetails.getAllocations().getVolumeUnit()) {
                BigDecimal val = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAchievedQuantities().getConsolidatedVolume(), consolidationDetails.getAchievedQuantities().getConsolidatedVolumeUnit(), consolidationDetails.getAllocations().getVolumeUnit()).toString());
                consolidationDetails.getAchievedQuantities().setConsolidatedVolume(val);
                consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(consolidationDetails.getAllocations().getVolumeUnit());
            }
            consolidationDetails = calculateConsolUtilization(consolidationDetails);
            ConsoleCalculationsResponse response = getConsoleCalculationsResponse(consolidationDetails);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateChargeable(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ConsolidationDetails consolidationDetails = getConsoleForCalculations((ConsoleCalculationsRequest) commonRequestModel.getData());
            String transportMode = consolidationDetails.getTransportMode();
            if(consolidationDetails.getAllocations() == null)
                consolidationDetails.setAllocations(new Allocations());
            BigDecimal weight = consolidationDetails.getAllocations().getWeight();
            String weightUnit = consolidationDetails.getAllocations().getWeightUnit();
            BigDecimal volume = consolidationDetails.getAllocations().getVolume();
            String volumeUnit = consolidationDetails.getAllocations().getVolumeUnit();
            if (weightUnit != null && volumeUnit != null) {
                VolumeWeightChargeable vwOb = calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);
                consolidationDetails.getAllocations().setChargable(vwOb.getChargeable());
                if (transportMode == Constants.TRANSPORT_MODE_AIR) {
                    BigDecimal charge = consolidationDetails.getAllocations().getChargable();
                    BigDecimal half = new BigDecimal("0.50");
                    BigDecimal floor = charge.setScale(0, BigDecimal.ROUND_FLOOR);
                    if (charge.subtract(half).compareTo(floor) <= 0 && charge.compareTo(floor) != 0) {
                        charge = floor.add(half);
                    } else {
                        charge = charge.setScale(0, BigDecimal.ROUND_CEILING);
                    }
                    consolidationDetails.getAllocations().setChargable(charge);
                }
                if (transportMode.equals(Constants.TRANSPORT_MODE_SEA) && consolidationDetails.getContainerCategory().equals(Constants.SHIPMENT_TYPE_LCL)) {
                    volume = new BigDecimal(convertUnit(Constants.VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());
                    weight = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
                    consolidationDetails.getAllocations().setChargable(weight.divide(new BigDecimal("1000")).max(volume));
                    vwOb = calculateVolumeWeight(transportMode, Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, weight, volume);
                }
                consolidationDetails.getAllocations().setChargeableUnit(vwOb.getChargeableUnit());
            }
            ConsoleCalculationsResponse response = getConsoleCalculationsResponse(consolidationDetails);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void setDescGoodsAndhandlingInfoMap(Map<Long, String> descOfGoodsMap, Map<Long, String> handlingInfoMap, Map<Long, String> hblNumberMap, Long containerId, String goodsDesc, String handlingInfo, String hblNumber) {
        if(!IsStringNullOrEmpty(goodsDesc)) {
            if(descOfGoodsMap.containsKey(containerId))
                descOfGoodsMap.put(containerId, descOfGoodsMap.get(containerId) + "\n" + goodsDesc);
            else
                descOfGoodsMap.put(containerId, goodsDesc);
        }
        if(!IsStringNullOrEmpty(handlingInfo)) {
            if(handlingInfoMap.containsKey(containerId))
                handlingInfoMap.put(containerId, handlingInfoMap.get(containerId) + "\n" + handlingInfo);
            else
                handlingInfoMap.put(containerId, handlingInfo);
        }
        if(!IsStringNullOrEmpty(hblNumber)) {
            if(hblNumberMap.containsKey(containerId))
                hblNumberMap.put(containerId, hblNumberMap.get(containerId) + ", " + hblNumber);
            else
                hblNumberMap.put(containerId, hblNumber);
        }
    }

    private void calculateDescOfGoodsAndHandlingInfo(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, boolean isAutoUpdate) {
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        Map<Long, String> descOfGoodsMap = new HashMap<>();
        Map<Long, String> handlingInfoMap = new HashMap<>();
        Map<Long, String> hblNumberMap = new HashMap<>();
        Map<Long, ShipmentDetails> shipmentNumberMap = new HashMap<>();
        boolean lcl = shipmentSettingsDetails.getMultipleShipmentEnabled() != null && shipmentSettingsDetails.getMultipleShipmentEnabled();
        boolean autoUpdate = isAutoUpdate || (consolidationDetails.getAutoUpdateGoodsDesc() != null && consolidationDetails.getAutoUpdateGoodsDesc());
        Set<Long> containerSelfDataAdded = new HashSet<>();
        if(consolidationDetails.getShipmentsList() != null && consolidationDetails.getShipmentsList().size() > 0) {
            for(ShipmentDetails shipmentDetails : consolidationDetails.getShipmentsList()) {
                shipmentNumberMap.put(shipmentDetails.getId(), shipmentDetails);
                boolean setContData = autoUpdate;
                if(autoUpdate && lcl) {
                    if(shipmentDetails.getContainerAutoWeightVolumeUpdate() != null && shipmentDetails.getContainerAutoWeightVolumeUpdate()) {
                        if(shipmentDetails.getPackingList() != null && shipmentDetails.getPackingList().size() > 0) {
                            for (Packing packing : shipmentDetails.getPackingList()) {
                                if(packing.getContainerId() != null) {
                                    setDescGoodsAndhandlingInfoMap(descOfGoodsMap, handlingInfoMap, hblNumberMap, packing.getContainerId(), packing.getGoodsDescription(), packing.getHandlingInfo(), null);
                                }
                            }
                        }
                        setContData = false;
                    }
                }
                if(setContData || !IsStringNullOrEmpty(shipmentDetails.getHouseBill())) {
                    if(shipmentDetails.getContainersList() != null && shipmentDetails.getContainersList().size() > 0) {
                        for(Containers containers : shipmentDetails.getContainersList()) {
                            if(setContData && !containerSelfDataAdded.contains(containers.getId()) && lcl) {
                                containerSelfDataAdded.add(containers.getId());
                                setDescGoodsAndhandlingInfoMap(descOfGoodsMap, handlingInfoMap, hblNumberMap, containers.getId(), containers.getDescriptionOfGoods(), containers.getHandlingInfo(), shipmentDetails.getHouseBill());
                            }
                            else
                                setDescGoodsAndhandlingInfoMap(descOfGoodsMap, handlingInfoMap, hblNumberMap, containers.getId(), null, null, shipmentDetails.getHouseBill());
                        }
                    }
                }

            }
        }

        if(consolidationDetailsResponse.getContainersList() != null && consolidationDetailsResponse.getContainersList().size() > 0) {
            for (ContainerResponse containerResponse : consolidationDetailsResponse.getContainersList()) {
                Map<String, String> tempMap = new HashMap<>();
                if(descOfGoodsMap.containsKey(containerResponse.getId()))
                    tempMap.put("descriptionOfGoods", descOfGoodsMap.get(containerResponse.getId()));
                else
                    tempMap.put("descriptionOfGoods", containerResponse.getDescriptionOfGoods());
                if(handlingInfoMap.containsKey(containerResponse.getId()))
                    tempMap.put("handlingInfo", handlingInfoMap.get(containerResponse.getId()));
                else
                    tempMap.put("handlingInfo", containerResponse.getHandlingInfo());
                if(hblNumberMap.containsKey(containerResponse.getId()))
                    containerResponse.setHblNumber(hblNumberMap.get(containerResponse.getId()));
                containerResponse.setTextFieldData(tempMap);
            }
        }

        if(!isAutoUpdate && consolidationDetailsResponse.getPackingList() != null && !consolidationDetailsResponse.getPackingList().isEmpty()) {
            for (PackingResponse packingResponse : consolidationDetailsResponse.getPackingList()) {
                if(packingResponse.getShipmentId() != null && shipmentNumberMap.containsKey(packingResponse.getShipmentId())) {
                    packingResponse.setShipmentNumber(shipmentNumberMap.get(packingResponse.getShipmentId()).getShipmentId());
                    packingResponse.setShipmentHazardous(shipmentNumberMap.get(packingResponse.getShipmentId()).getContainsHazardous());
                }
            }
        }
    }

    public VolumeWeightChargeable calculateVolumeWeight(String transportMode, String weightUnit, String volumeUnit, BigDecimal weight, BigDecimal volume) throws RunnerException {
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
                    default:
                }
            }
            return vwOb;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateAchievedValues(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ShipmentGridChangeResponse response = new ShipmentGridChangeResponse();
            Long consolidationId = commonRequestModel.getId();
            ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(consolidationId).get();
            calculateAchievedValues(consolidationDetails, response, consolidationDetails.getShipmentsList());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void calculateAchievedValues(ConsolidationDetails consolidationDetails, ShipmentGridChangeResponse response, List<ShipmentDetails> shipmentDetailsList) throws Exception {
        if (consolidationDetails.getOverride() != null && consolidationDetails.getOverride()) {
            return;
        }
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        String weightChargeableUnit = Constants.WEIGHT_UNIT_KG;
        if(!IsStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()))
            weightChargeableUnit = shipmentSettingsDetails.getWeightChargeableUnit();
        String volumeChargeableUnit = Constants.VOLUME_UNIT_M3;
        if(!IsStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()))
            volumeChargeableUnit = shipmentSettingsDetails.getVolumeChargeableUnit();
        BigDecimal sumWeight = new BigDecimal(0);
        BigDecimal sumVolume = new BigDecimal(0);
        if (shipmentDetailsList != null && !shipmentDetailsList.isEmpty()) {
            for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
                sumWeight = sumWeight.add(new BigDecimal(convertUnit(Constants.MASS, shipmentDetails.getWeight(), shipmentDetails.getWeightUnit(), weightChargeableUnit).toString()));
                sumVolume = sumVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, shipmentDetails.getVolume(), shipmentDetails.getVolumeUnit(), volumeChargeableUnit).toString()));
            }
            response.setSummaryShipmentsCount(shipmentDetailsList.size());
        }
        else {
            response.setSummaryShipmentsCount(0);
        }
        Double consoleTeu = 0.0;
        Double shipmentTeu = 0.0;
        long consoleCont = 0l;
        long shipmentCont = 0l;
        if(consolidationDetails.getContainersList() != null && consolidationDetails.getContainersList().size() > 0) {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            List<String> containerTypes = new ArrayList<>();
            List<ContainerResponse> containerResponseList = jsonHelper.convertValueToList(consolidationDetails.getContainersList(), ContainerResponse.class);
            if (!Objects.isNull(containerResponseList))
                containerResponseList.forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId() )));
            Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE);

            for(Containers containers : consolidationDetails.getContainersList()) {
                var cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).get(keyGenerator.customCacheKeyForMasterData(CacheConstants.CONTAINER_TYPE, containers.getContainerCode()));
                EntityTransferContainerType object = (EntityTransferContainerType) cache.get();
                if(containers.getContainerCount() != null) {
                    consoleCont = consoleCont + containers.getContainerCount();
                    if(containers.getShipmentsList() != null && containers.getShipmentsList().size() > 0) {
                        shipmentCont = shipmentCont + containers.getContainerCount();
                    }
                    if(object != null && object.getTeu() != null) {
                        consoleTeu = consoleTeu + (containers.getContainerCount() * object.getTeu());
                        if(containers.getShipmentsList() != null && containers.getShipmentsList().size() > 0) {
                            shipmentTeu = shipmentTeu + (containers.getContainerCount() * object.getTeu());
                        }
                    }
                }
            }
        }
        response.setSummaryConsoleTEU(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(consoleTeu), 0, v1TenantSettingsResponse));
        response.setSummaryConsolContainer(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(consoleCont), 0, v1TenantSettingsResponse));
        response.setSummaryShipmentTEU(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(shipmentTeu), 0, v1TenantSettingsResponse));
        response.setSummaryShipmentContainer(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(shipmentCont), 0, v1TenantSettingsResponse));

        if(consolidationDetails.getAchievedQuantities() == null)
            consolidationDetails.setAchievedQuantities(new AchievedQuantities());
        consolidationDetails.getAchievedQuantities().setConsolidatedWeight(sumWeight);
        consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(weightChargeableUnit);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolume(sumVolume);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(volumeChargeableUnit);

        consolidationDetails = calculateConsolUtilization(consolidationDetails);

        String transportMode = consolidationDetails.getTransportMode();
        if(consolidationDetails.getAllocations() == null)
            consolidationDetails.setAllocations(new Allocations());
        VolumeWeightChargeable vwOb = calculateVolumeWeight(transportMode, weightChargeableUnit, volumeChargeableUnit, sumWeight, sumVolume);

        consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantity(vwOb.getChargeable());
        consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantityUnit(vwOb.getChargeableUnit());
        if (transportMode.equals(Constants.TRANSPORT_MODE_SEA) &&
                consolidationDetails.getContainerCategory() != null && consolidationDetails.getContainerCategory().equals(Constants.SHIPMENT_TYPE_LCL)) {
            BigDecimal winKg = new BigDecimal(convertUnit(Constants.MASS, consolidationDetails.getAllocations().getWeight(), consolidationDetails.getAllocations().getWeightUnit(), Constants.WEIGHT_UNIT_KG).toString());
            BigDecimal vinM3 = new BigDecimal(convertUnit(Constants.VOLUME, consolidationDetails.getAllocations().getVolume(), consolidationDetails.getAllocations().getVolumeUnit(), Constants.VOLUME_UNIT_M3).toString());
            consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantity(winKg.divide(BigDecimal.valueOf(1000)).max(vinM3));
            consolidationDetails.getAchievedQuantities().setConsolidationChargeQuantityUnit(Constants.VOLUME_UNIT_M3);
        }
        consolidationDetails.getAchievedQuantities().setWeightVolume(vwOb.getVolumeWeight());
        consolidationDetails.getAchievedQuantities().setWeightVolumeUnit(vwOb.getVolumeWeightUnit());
        consolidationDetails.getAllocations().setChargeableUnit(vwOb.getChargeableUnit());

        response.setAllocations(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));
        response.setSummaryWeight(IReport.ConvertToWeightNumberFormat(sumWeight, v1TenantSettingsResponse) + " " + weightChargeableUnit);
        response.setSummaryVolume(IReport.ConvertToVolumeNumberFormat(sumVolume, v1TenantSettingsResponse) + " " + volumeChargeableUnit);
        if(!IsStringNullOrEmpty(consolidationDetails.getContainerCategory()) && consolidationDetails.getContainerCategory().equals(Constants.SHIPMENT_TYPE_LCL)
                && !IsStringNullOrEmpty(consolidationDetails.getTransportMode()) && consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
            double volInM3 = convertUnit(Constants.VOLUME, sumVolume, volumeChargeableUnit, Constants.VOLUME_UNIT_M3).doubleValue();
            double wtInKg = convertUnit(Constants.MASS, sumWeight, weightChargeableUnit, Constants.WEIGHT_UNIT_KG).doubleValue();
            double chargeableWeight = Math.max(wtInKg / 1000, volInM3);
            chargeableWeight = BigDecimal.valueOf(chargeableWeight).setScale(2, RoundingMode.HALF_UP).doubleValue();
            response.setSummaryChargeableWeight(chargeableWeight + " " + Constants.VOLUME_UNIT_M3);
        }
    }

    public ResponseEntity<IRunnerResponse> calculateContainerSummary(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        CalculateContainerSummaryRequest request = (CalculateContainerSummaryRequest) commonRequestModel.getData();
        try {
            List<Containers> containers = jsonHelper.convertValueToList(request.getContainersList(), Containers.class);
            ContainerSummaryResponse response = containerService.calculateContainerSummary(containers, request.getTransportMode(), request.getContainerCategory());
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> calculatePackSummary(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        CalculatePackSummaryRequest request = (CalculatePackSummaryRequest) commonRequestModel.getData();
        try {
            List<Packing> packingList = jsonHelper.convertValueToList(request.getPackingList(), Packing.class);
            PackSummaryResponse response = packingService.calculatePackSummary(packingList, request.getTransportMode(), request.getContainerCategory(), new ShipmentMeasurementDetailsDto());
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> listPacksForAssignDetach(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ConsolePacksListRequest request = (ConsolePacksListRequest) commonRequestModel.getData();
            ConsolePacksListResponse response = new ConsolePacksListResponse();
            response.setPacksList(new ArrayList<>());
            int size = 1;
            response.setIsFCL(false);
            if(request.getIsAssign()) {
                List<Long> shipIds = new ArrayList<>();
                List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(request.getConsolidationId());
                if(consoleShipmentMappings != null && consoleShipmentMappings.size() > 0)
                    shipIds = consoleShipmentMappings.stream().map(e -> e.getShipmentId()).toList();
                ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, shipIds, "IN", null);
                listCommonRequest = andCriteria(Constants.CONTAINER_ID, "", "ISNULL", listCommonRequest);
                Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());

                List<Long> contShipIds = new ArrayList<>();
                List<ShipmentsContainersMapping> shipmentsContainersMappingList = shipmentsContainersMappingDao.findByContainerId(request.getContainerId());
                if(shipmentsContainersMappingList != null && shipmentsContainersMappingList.size() > 0) {
                    contShipIds = shipmentsContainersMappingList.stream().map(e -> e.getShipmentId()).toList();
                }

                Map<Long, ShipmentDetails> map = new HashMap<>();
                List<Long> shipmentsIncluded = new ArrayList<>();
                if(packings != null && packings.getContent() != null && packings.getContent().size() > 0) {
                    shipmentsIncluded = packings.getContent().stream().map(e -> e.getShipmentId()).toList();
                    for (Packing packing : packings) {
                        ConsolePacksListResponse.PacksList packsList = jsonHelper.convertValue(packing, ConsolePacksListResponse.PacksList.class);
                        ShipmentDetails shipmentDetails = getShipmentFromMap(map, packing.getShipmentId());
                        if(shipmentDetails != null) {
                            packsList.setShipmentClient(jsonHelper.convertValue(shipmentDetails.getClient(), PartiesResponse.class));
                            packsList.setShipmentHouseBill(shipmentDetails.getHouseBill());
                            packsList.setShipmentMasterBill(shipmentDetails.getMasterBill());
                            packsList.setShipmentNumber(shipmentDetails.getShipmentId());
                            packsList.setShipmentType(shipmentDetails.getShipmentType());
                        }
                        response.getPacksList().add(packsList);
                        size++;
                    }
                }
                for(Long shipmentId: shipIds) {
                    if(shipmentsIncluded.indexOf(shipmentId) < 0 && contShipIds.indexOf(shipmentId) < 0) {
                        ShipmentDetails shipmentDetails = getShipmentFromMap(map, shipmentId);
                        if(shipmentDetails != null && shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                            ConsolePacksListResponse.PacksList packsList = new ConsolePacksListResponse.PacksList();
                            packsList.setShipmentId(shipmentDetails.getId());
                            packsList.setShipmentNumber(shipmentDetails.getShipmentId());
                            packsList.setId(size * -1L);
                            packsList.setShipmentHouseBill(shipmentDetails.getHouseBill());
                            packsList.setShipmentMasterBill(shipmentDetails.getMasterBill());
                            packsList.setShipmentType(shipmentDetails.getShipmentType());
                            packsList.setShipmentClient(jsonHelper.convertValue(shipmentDetails.getClient(), PartiesResponse.class));
                            response.getPacksList().add(packsList);
                            size++;
                        }
                    }
                }
            }
            else {
                List<Long> shipIds = new ArrayList<>();
                List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(request.getConsolidationId());
                if(consoleShipmentMappings != null && consoleShipmentMappings.size() > 0)
                    shipIds = consoleShipmentMappings.stream().map(e -> e.getShipmentId()).toList();
                ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, shipIds, "IN", null);
                listCommonRequest = andCriteria(Constants.CONTAINER_ID, request.getContainerId(), "=", listCommonRequest);
                Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());

                List<Long> contShipIds = new ArrayList<>();
                List<ShipmentsContainersMapping> shipmentsContainersMappingList = shipmentsContainersMappingDao.findByContainerId(request.getContainerId());
                if(shipmentsContainersMappingList != null && shipmentsContainersMappingList.size() > 0) {
                    contShipIds = shipmentsContainersMappingList.stream().map(e -> e.getShipmentId()).toList();
                }
                Map<Long, ShipmentDetails> map = new HashMap<>();

                if(packings != null && packings.getContent() != null && packings.getContent().size() > 0) {
                    for (Packing packing : packings) {
                        ConsolePacksListResponse.PacksList packsList = jsonHelper.convertValue(packing, ConsolePacksListResponse.PacksList.class);
                        ShipmentDetails shipmentDetails = getShipmentFromMap(map, packing.getShipmentId());
                        if(shipmentDetails != null) {
                            packsList.setShipmentClient(jsonHelper.convertValue(shipmentDetails.getClient(), PartiesResponse.class));
                            packsList.setShipmentHouseBill(shipmentDetails.getHouseBill());
                            packsList.setShipmentMasterBill(shipmentDetails.getMasterBill());
                            packsList.setShipmentNumber(shipmentDetails.getShipmentId());
                            packsList.setShipmentType(shipmentDetails.getShipmentType());
                        }
                        response.getPacksList().add(packsList);
                        size++;
                    }
                }

                if(contShipIds != null && contShipIds.size() == 1 && (packings == null || packings.getContent() == null || packings.getContent().isEmpty())) {
                    for(Long shipmentId: shipIds) {
                        if(shipmentId.equals(contShipIds.get(0))) {
                            ShipmentDetails shipmentDetails = getShipmentFromMap(map, shipmentId);
                            if(shipmentDetails != null && shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                                response.setIsFCL(true);
                                ConsolePacksListResponse.PacksList packsList = new ConsolePacksListResponse.PacksList();
                                packsList.setShipmentId(shipmentDetails.getId());
                                packsList.setShipmentNumber(shipmentDetails.getShipmentId());
                                packsList.setId(size * -1L);
                                packsList.setShipmentHouseBill(shipmentDetails.getHouseBill());
                                packsList.setShipmentMasterBill(shipmentDetails.getMasterBill());
                                packsList.setShipmentType(shipmentDetails.getShipmentType());
                                packsList.setShipmentClient(jsonHelper.convertValue(shipmentDetails.getClient(), PartiesResponse.class));
                                response.getPacksList().add(packsList);
                                size++;
                            }
                        }
                    }
                }
            }
            try {
                var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addAllMasterDataInSingleCallPacksList(response)), executorService);
                var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addAllCommodityTypesInSingleCallPacksList(response)), executorService);
                CompletableFuture.allOf(masterListFuture, commodityTypesFuture).join();
            }
            catch (Exception e) {
                log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_CONSOLIDATION_RETRIEVE, e.getLocalizedMessage());
            }
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCallPacksList (ConsolePacksListResponse consolePacksListResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<MasterListRequest> listRequests = new ArrayList<>();
        if(!Objects.isNull(consolePacksListResponse.getPacksList()))
            consolePacksListResponse.getPacksList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, ConsolePacksListResponse.PacksList.class, fieldNameKeyMap, ConsolePacksListResponse.PacksList.class.getSimpleName() )));

        MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
        masterListRequestV2.setMasterListRequests(listRequests);
        masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));

        Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
        masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST);

        if(!Objects.isNull(consolePacksListResponse.getPacksList()))
            consolePacksListResponse.getPacksList().forEach(r -> r.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolePacksListResponse.PacksList.class.getSimpleName()), CacheConstants.MASTER_LIST)));

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCommodityTypesInSingleCallPacksList(ConsolePacksListResponse consolePacksListResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> commodityTypes = new HashSet<>();
        if(!Objects.isNull(consolePacksListResponse.getPacksList()))
            consolePacksListResponse.getPacksList().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, ConsolePacksListResponse.PacksList.class, fieldNameKeyMap, ConsolePacksListResponse.PacksList.class.getSimpleName() )));

        Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY);

        if(!Objects.isNull(consolePacksListResponse.getPacksList()))
            consolePacksListResponse.getPacksList().forEach(r -> r.setCommodityMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolePacksListResponse.PacksList.class.getSimpleName()), CacheConstants.COMMODITY)));

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    @Override
    public ResponseEntity<IRunnerResponse> assignPacksAndShipments(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ContainerShipmentADInConsoleRequest request = (ContainerShipmentADInConsoleRequest) commonRequestModel.getData();
            ContainerShipmentADInConsoleResponse response = new ContainerShipmentADInConsoleResponse();
            Optional<Containers> containersOpt = containerDao.findById(request.getContainer().getId());
            Containers container = null;
            if(containersOpt.isPresent()) {
                container = containersOpt.get();
            }
            if(container != null && request.getPacksList() != null && request.getPacksList().size() > 0) {
                BigDecimal weight = container.getAchievedWeight();
                BigDecimal volume = container.getAchievedVolume();
                if(weight == null) {
                    weight = BigDecimal.ZERO;
                    container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
                }
                if(volume == null) {
                    volume = BigDecimal.ZERO;
                    container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
                }
                List<Long> contShipIds = new ArrayList<>();
                if(container.getShipmentsList() != null && container.getShipmentsList().size() > 0)
                    contShipIds = container.getShipmentsList().stream().map(e -> e.getId()).toList();
                Set<Long> shipmentsIncluded = new HashSet<>();
                boolean isFCL = false;
                boolean isFCLAlready = contShipIds.size() == 1 && container.getShipmentsList().get(0).getShipmentType().equals(Constants.CARGO_TYPE_FCL);
                for(ContainerShipmentADInConsoleRequest.PacksList pack : request.getPacksList()) {
                    if(!shipmentsIncluded.contains(pack.getShipmentId()) && contShipIds.indexOf(pack.getShipmentId()) < 0)
                        shipmentsIncluded.add(pack.getShipmentId());
                    if(pack.getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                        isFCL = true;
                        if( (isFCL || isFCLAlready) && contShipIds.size() + shipmentsIncluded.size() > 1 ) {
                            throw new ValidationException("Mentioned container is already assigned or being assigned to a FCL Shipment - " + pack.getShipmentNumber() + " along with some other Shipment. Please check and retry!");
                        }
                    }
                    if(pack.getWeight() != null && !IsStringNullOrEmpty(pack.getWeightUnit()) && !IsStringNullOrEmpty(container.getAchievedWeightUnit()))
                        weight = weight.add(new BigDecimal(convertUnit(Constants.MASS, pack.getWeight(), pack.getWeightUnit(), container.getAchievedWeightUnit()).toString()));
                    if(pack.getVolume() != null && !IsStringNullOrEmpty(pack.getVolumeUnit()) && !IsStringNullOrEmpty(container.getAchievedVolumeUnit()))
                        volume = volume.add(new BigDecimal(convertUnit(Constants.VOLUME, pack.getVolume(), pack.getVolumeUnit(), container.getAchievedVolumeUnit()).toString()));
                }
                if( (isFCL || isFCLAlready) && contShipIds.size() + shipmentsIncluded.size() > 1 ) {
                    if(container.getShipmentsList().size() > 0 && container.getShipmentsList().get(0).getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                        throw new ValidationException("Mentioned container is already assigned or being assigned to a FCL Shipment - " + container.getShipmentsList().get(0).getShipmentId() + " along with some other Shipment. Please check and retry!");
                    }
                }
                if(isFCL || isFCLAlready) {
                    weight = container.getAllocatedWeight();
                    volume = container.getAllocatedVolume();
                }
                ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
                if(shipmentSettingsDetails.getIsConsolidator() != null && shipmentSettingsDetails.getIsConsolidator()
                && ((weight != null && weight.compareTo(container.getAllocatedWeight()) > 0) || (volume != null && volume.compareTo(container.getAllocatedVolume()) > 0))) {
                    if(request.getIsConfirmedByUser() != null && request.getIsConfirmedByUser().booleanValue()) {
                        container = attachContainer(request, shipmentsIncluded, container, weight, volume);
                    }
                    else {
                        response.setRequireConfirmationFromUser(true);
                        return ResponseHelper.buildSuccessResponse(response);
                    }
                }
                else {
                    container = attachContainer(request, shipmentsIncluded, container, weight, volume);
                }
            }
            else {
                response.setContainer(jsonHelper.convertValue(request.getContainer(), ContainerResponse.class));
            }
            response.setRequireConfirmationFromUser(false);
            response.setContainer(jsonHelper.convertValue(container, ContainerResponse.class));
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private Containers attachContainer(ContainerShipmentADInConsoleRequest request, Set<Long> newShipmentsIncluded, Containers container, BigDecimal weight, BigDecimal volume) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("id", request.getPacksList().stream().filter(e -> e.getId() != null && e.getId() > 0).map(e -> e.getId()).toList(), "IN");
        Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
        Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
        List<Packing> packingList = null;
        if(packings != null && packings.getContent() != null && packings.getContent().size() > 0) {
            packingList = packings.getContent();
            for(Packing pack: packingList) {
                pack.setContainerId(container.getId());
            }
            packingDao.saveAll(packingList);
        }
        container.setAchievedWeight(weight);
        container.setAchievedVolume(volume);
        container = containerService.calculateUtilization(container);
        containerDao.save(container);
        shipmentsContainersMappingDao.assignShipments(container.getId(), newShipmentsIncluded.stream().toList(), false);
        try {
            packingsADSync.sync(packingList, UUID.randomUUID().toString());
        }
        catch (Exception e) {
            log.error(SyncingConstants.ERROR_SYNCING_PACKS);
        }
        return container;
    }

    @Override
    public ResponseEntity<IRunnerResponse> detachPacksAndShipments(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ContainerShipmentADInConsoleRequest request = (ContainerShipmentADInConsoleRequest) commonRequestModel.getData();
            ContainerResponse response = null;
            Optional<Containers> containersOpt = containerDao.findById(request.getContainer().getId());
            Containers container = null;
            if(containersOpt.isPresent()) {
                container = containersOpt.get();
            }
            if(container != null && request.getPacksList() != null && request.getPacksList().size() > 0) {
                BigDecimal weight = container.getAchievedWeight();
                BigDecimal volume = container.getAchievedVolume();
                if(weight == null) {
                    weight = BigDecimal.ZERO;
                    container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
                }
                if(volume == null) {
                    volume = BigDecimal.ZERO;
                    container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
                }
                Set<Long> shipmentdIdSet = new HashSet<>();
                List<Packing> packingList = new ArrayList<>();
                for (ContainerShipmentADInConsoleRequest.PacksList packing : request.getPacksList()) {
                    shipmentdIdSet.add(packing.getShipmentId());
                    if(packing.getId() > 0) {
                        Packing pack = null;
                        pack = packingDao.findById(packing.getId()).get();
                        if(!packing.getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                            if(pack.getWeight() != null && !IsStringNullOrEmpty(pack.getWeightUnit()) && !IsStringNullOrEmpty(container.getAchievedWeightUnit()))
                                weight = weight.subtract(new BigDecimal(convertUnit(Constants.MASS, pack.getWeight(), pack.getWeightUnit(), container.getAchievedWeightUnit()).toString()));
                            if(pack.getVolume() != null && !IsStringNullOrEmpty(pack.getVolumeUnit()) && !IsStringNullOrEmpty(container.getAchievedVolumeUnit()))
                                volume = volume.subtract(new BigDecimal(convertUnit(Constants.VOLUME, pack.getVolume(), pack.getVolumeUnit(), container.getAchievedVolumeUnit()).toString()));
                        }
                        pack.setContainerId(null);
                        packingDao.save(pack);
                        packingList.add(pack);
                    }
                }
                container.setAchievedWeight(weight);
                container.setAchievedVolume(volume);
                container = containerService.calculateUtilization(container);
                Set<Long> removeShipmentIds = new HashSet<>();
                if(shipmentdIdSet.size() > 0) {
                    for(Long shipmentId : shipmentdIdSet) {
                        ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, shipmentId, "=", null);
                        listCommonRequest = andCriteria(Constants.CONTAINER_ID, container.getId(), "=", listCommonRequest);
                        Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                        Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
                        if(packings == null || packings.getContent() == null || packings.getContent().isEmpty())
                            removeShipmentIds.add(shipmentId);
                    }
                }
                if(removeShipmentIds.size() == 1 && container.getShipmentsList() != null && container.getShipmentsList().size() == 1) {
                    if(request.getIsFCL() || container.getShipmentsList().get(0).getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                        container.setAchievedVolume(BigDecimal.ZERO);
                        container.setAchievedWeight(BigDecimal.ZERO);
                        container.setWeightUtilization("0");
                        container.setVolumeUtilization("0");
                    }
                }
                container = containerDao.save(container);
                shipmentsContainersMappingDao.detachShipments(container.getId(), removeShipmentIds.stream().toList(), false);
                containerService.afterSave(container, false);
                try {
                    packingsADSync.sync(packingList, UUID.randomUUID().toString());
                }
                catch (Exception e) {
                    log.error(SyncingConstants.ERROR_SYNCING_PACKS);
                }
                response = jsonHelper.convertValue(container, ContainerResponse.class);
            }
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private ShipmentDetails getShipmentFromMap(Map<Long, ShipmentDetails> map, Long id) {
        ShipmentDetails shipmentDetails = null;
        if(map.containsKey(id))
            shipmentDetails = map.get(id);
        else {
            Optional<ShipmentDetails> shipmentDetails1 = shipmentDao.findById(id);
            if(shipmentDetails1 != null && shipmentDetails1.isPresent()) {
                shipmentDetails = shipmentDetails1.get();
                map.put(shipmentDetails.getId(), shipmentDetails);
            }
        }
        return shipmentDetails;
    }

    private Containers getContainerFromMap(Map<Long, Containers> map, Long id) {
        Containers containers = null;
        if(map.containsKey(id))
            containers = map.get(id);
        else {
            Optional<Containers> containers1 = containerDao.findById(id);
            if(containers1 != null && containers1.isPresent()) {
                containers = containers1.get();
                map.put(containers.getId(), containers);
            }
        }
        return containers;
    }

    public ResponseEntity<IRunnerResponse> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException {
        try {
            // create common list request for consolidation id
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for  Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Consolidation complete retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            CompletableFuture<ResponseEntity<IRunnerResponse>> consolidationsFuture = retrieveByIdAsync(commonRequestModel);
            RunnerResponse<ConsolidationDetailsResponse> res = (RunnerResponse<ConsolidationDetailsResponse>) consolidationsFuture.get().getBody();
            if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
                return ResponseHelper.buildSuccessResponse(res.getData());
            else{
                return ResponseHelper.buildSuccessResponse(PartialFetchUtils.fetchPartialListData(res.getData(), request.getIncludeColumns()));
            }
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> fullConsolidationsList(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(CONSOLIDATION_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(CONSOLIDATION_LIST_REQUEST_NULL_ERROR);
            }
            checkBookingIdCriteria(request);
            Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
            Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildListSuccessResponse(
                        convertEntityListToFullConsolidationList(consolidationDetailsPage.getContent()),
                        consolidationDetailsPage.getTotalPages(),
                        consolidationDetailsPage.getTotalElements());
            else {
                List<IRunnerResponse>filteredList=new ArrayList<>();
                for( var curr: convertEntityListToFullConsolidationList(consolidationDetailsPage.getContent())){
                    RunnerPartialListResponse res=new RunnerPartialListResponse();
                    res.setData(PartialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
                    filteredList.add( res);
                }
                return ResponseHelper.buildListSuccessResponse(
                        filteredList,
                        consolidationDetailsPage.getTotalPages(),
                        consolidationDetailsPage.getTotalElements());
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<IRunnerResponse> convertEntityListToFullConsolidationList(List<ConsolidationDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(consolidationDetails -> {
            ConsolidationDetailsResponse response = modelMapper.map(consolidationDetails, ConsolidationDetailsResponse.class);
            responseList.add(response);
        });
        return responseList;
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(CONSOLIDATION_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(CONSOLIDATION_LIST_REQUEST_NULL_ERROR);
            }
            checkBookingIdCriteria(request);
            Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
            Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            if(request.getIncludeColumns() == null || request.getIncludeColumns().isEmpty()) {
                List<IRunnerResponse> consoleResponse = convertEntityListToDtoList(consolidationDetailsPage.getContent());
                log.info("Consolidation list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildListSuccessResponse(
                        consoleResponse,
                        consolidationDetailsPage.getTotalPages(),
                        consolidationDetailsPage.getTotalElements());
            }
            else {
                List<IRunnerResponse>filteredList=new ArrayList<>();
                for( var curr: convertEntityListToDtoList(consolidationDetailsPage.getContent())){
                    RunnerPartialListResponse res=new RunnerPartialListResponse();
                    res.setData(PartialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
                    filteredList.add( res);
                }
                return ResponseHelper.buildListSuccessResponse(
                        filteredList,
                        consolidationDetailsPage.getTotalPages(),
                        consolidationDetailsPage.getTotalElements());
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    private void checkBookingIdCriteria(ListCommonRequest request)
    {
        if(request != null && request.getFilterCriteria() != null && request.getFilterCriteria().size() > 0)
        {
            checkForBookingIdFilter(request.getFilterCriteria());
        }
    }

    private void checkForBookingIdFilter(List<FilterCriteria> filterCriteriaList) {
        for(FilterCriteria filterCriteria: filterCriteriaList)
        {
            if(filterCriteria.getCriteria() != null && filterCriteria.getCriteria().getFieldName() != null &&
                    filterCriteria.getCriteria().getFieldName().equals("bookingId") && filterCriteria.getCriteria().getValue() != null) {

                ConsoleBookingIdFilterRequest consoleBookingIdFilterRequest = new ConsoleBookingIdFilterRequest();
                consoleBookingIdFilterRequest.setIntraBookingId(filterCriteria.getCriteria().getValue().toString());
                GuidsListResponse guidsListResponse = v1Service.fetchBookingIdFilterGuids(consoleBookingIdFilterRequest);
                filterCriteria.getCriteria().setFieldName("guid");
                filterCriteria.getCriteria().setOperator("IN");
                filterCriteria.getCriteria().setValue(guidsListResponse.getGuidsList());
            }
            if(filterCriteria.getInnerFilter() != null && filterCriteria.getInnerFilter().size() > 0) {
                checkForBookingIdFilter(filterCriteria.getInnerFilter());
            }
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Consolidation async list for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
            Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Consolidation async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(consolidationDetailsPage.getContent()),
                    consolidationDetailsPage.getTotalPages(),
                    consolidationDetailsPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }

    }

    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement Validation logic
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.debug("Request is empty for Consolidation delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for Consolidation delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(id);
            if (!consolidationDetails.isPresent()) {
                log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_ERROR_WITH_REQUEST_ID, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            String oldEntityJsonString = jsonHelper.convertToJson(consolidationDetails.get());
            consolidationDetailsDao.delete(consolidationDetails.get());
            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, ConsolidationDetails.class))
                            .parent(ConsolidationDetails.class.getSimpleName())
                            .parentId(consolidationDetails.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );

            log.info("Deleted Consolidation details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request.getId() == null && request.getGuid() == null) {
                log.error("Request Id and Guid are null for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException("Id and GUID can't be null. Please provide any one !");
            }
            Long id = request.getId();
            Optional<ConsolidationDetails> consolidationDetails = Optional.ofNullable(null);
            if(id != null ){
                consolidationDetails = consolidationDetailsDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                consolidationDetails = consolidationDetailsDao.findByGuid(guid);
            }
            if (!consolidationDetails.isPresent()) {
                log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_ERROR_WITH_REQUEST_ID, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info(ConsolidationConstants.CONSOLIDATION_DETAILS_FETCHED_SUCCESSFULLY, id, LoggerHelper.getRequestIdFromMDC());
            ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
            if(Boolean.TRUE.equals(shipmentSettingsDetails.getMergeContainers()) && consolidationDetails.get().getContainersList() != null && !consolidationDetails.get().getContainersList().isEmpty()) {
                consolidationDetails.get().setContainersList(mergeContainers(consolidationDetails.get().getContainersList(), shipmentSettingsDetails));
            }
            ConsolidationDetailsResponse response = jsonHelper.convertValue(consolidationDetails.get(), ConsolidationDetailsResponse.class);
            createConsolidationPayload(consolidationDetails.get(), response);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Async
    public CompletableFuture<ResponseEntity<IRunnerResponse>> retrieveByIdAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Consolidation async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Consolidation async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(id);
            if (!consolidationDetails.isPresent()) {
                log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_ERROR_WITH_REQUEST_ID, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Consolidation details async fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
            if(Boolean.TRUE.equals(shipmentSettingsDetails.getMergeContainers()) && consolidationDetails.get().getContainersList() != null && !consolidationDetails.get().getContainersList().isEmpty()) {
                consolidationDetails.get().setContainersList(mergeContainers(consolidationDetails.get().getContainersList(), shipmentSettingsDetails));
            }
            ConsolidationDetailsResponse response = jsonHelper.convertValue(consolidationDetails.get(), ConsolidationDetailsResponse.class);
            createConsolidationPayload(consolidationDetails.get(), response);
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(response));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getAllMasterData(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            Long id = commonRequestModel.getId();
            Optional<ConsolidationDetails> consolidationDetailsOptional = consolidationDetailsDao.findById(id);
            if(!consolidationDetailsOptional.isPresent()) {
                log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_FOR_GIVEN_ID_ERROR, id);
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ConsolidationDetails consolidationDetails = consolidationDetailsOptional.get();
            ConsolidationDetailsResponse consolidationDetailsResponse = jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
            Map<String, Object> response = fetchAllMasterDataByKey(consolidationDetails, consolidationDetailsResponse);
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public Map<String, Object> fetchAllMasterDataByKey(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse) {
        Map<String, Object> response = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllUnlocationDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCarrierDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCurrencyDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCommodityTypesInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllWarehouseDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addAllVesselDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var dgSubstanceFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> addAllDGSubstanceDataInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllContainerTypesInSingleCall(consolidationDetails, consolidationDetailsResponse, response)), executorService);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture, vesselDataFuture, dgSubstanceFuture, containerTypeFuture).join();

        return response;
    }

    public void createConsolidationPayload(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse) {
        try {
            double _start = System.currentTimeMillis();
            var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCall(consolidationDetails, consolidationDetailsResponse, null)), executorService);
            var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllUnlocationDataInSingleCall(consolidationDetails, consolidationDetailsResponse, null)), executorService);
            var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCarrierDataInSingleCall(consolidationDetails, consolidationDetailsResponse, null)), executorService);
            var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCurrencyDataInSingleCall(consolidationDetails, consolidationDetailsResponse, null)), executorService);
            var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCommodityTypesInSingleCall(consolidationDetails, consolidationDetailsResponse, null)), executorService);
            var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(consolidationDetails, consolidationDetailsResponse, null)), executorService);
            var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllWarehouseDataInSingleCall(consolidationDetails, consolidationDetailsResponse, null)), executorService);
            var calculationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.calculationsOnRetrieve(consolidationDetails, consolidationDetailsResponse)), executorService);
            CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture, calculationsFuture).join();
            if(consolidationDetails.getBookingStatus() != null && Arrays.stream(CarrierBookingStatus.values()).map(CarrierBookingStatus::name).toList().contains(consolidationDetails.getBookingStatus()))
                consolidationDetailsResponse.setBookingStatus(CarrierBookingStatus.valueOf(consolidationDetails.getBookingStatus()).getDescription());
            log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.CONSOLE_RETRIEVE_COMPLETE_MASTER_DATA, (System.currentTimeMillis() - _start) , LoggerHelper.getRequestIdFromMDC());
            if(consolidationDetailsResponse.getId() != null) {
                var awb = awbDao.findByConsolidationId(consolidationDetailsResponse.getId());
                if (awb != null && !awb.isEmpty()) {
                    if (awb.get(0).getAirMessageStatus() != null)
                        consolidationDetailsResponse.setAwbStatus(awb.get(0).getAirMessageStatus());
                    else
                        consolidationDetailsResponse.setAwbStatus(AwbStatus.AWB_GENERATED);
                    if(awb.get(0).getLinkedHawbAirMessageStatus() != null)
                        consolidationDetailsResponse.setLinkedHawbStatus(awb.get(0).getLinkedHawbAirMessageStatus());
                }
            }
            if (Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_ROA))
                fetchTruckerInfo(consolidationDetails.getId(), consolidationDetailsResponse);
        }  catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_CONSOLIDATION_RETRIEVE, ex.getLocalizedMessage());
        }
    }

    private void fetchTruckerInfo(Long id, ConsolidationDetailsResponse consolidationDetailsResponse) {
        List<Long> shipmentIds = consoleShipmentMappingDao.findByConsolidationId(id).stream().map(ConsoleShipmentMapping::getShipmentId).collect(toList());
        List<TruckDriverDetailsResponse> truckingInfo = new ArrayList<>();
        if (!shipmentIds.isEmpty()) {
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentIds, "IN");
            Pair<Specification<TruckDriverDetails>, Pageable> pair = fetchData(listCommonRequest, TruckDriverDetails.class);
            Page<TruckDriverDetails> truckDriverDetailsPage = truckDriverDetailsDao.findAll(pair.getLeft(), pair.getRight());
            List<TruckDriverDetails> truckDriverDetails = truckDriverDetailsPage.stream().toList();
            if (!truckDriverDetails.isEmpty()) {
                ListCommonRequest listReq = constructListCommonRequest("id", shipmentIds, "IN");
                Pair<Specification<ShipmentDetails>, Pageable> pair1 = fetchData(listReq, ShipmentDetails.class);
                Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(pair1.getLeft(), pair1.getRight());
                var map = shipmentDetailsPage.getContent().stream().collect(toMap(ShipmentDetails::getId, ShipmentDetails::getShipmentId));
                List<ContainerResponse> containers = consolidationDetailsResponse.getContainersList();
                Map<Long, ContainerResponse> contMap = new HashMap<>();
                if(containers != null)
                    contMap = containers.stream().collect(Collectors.toMap(ContainerResponse::getId, Function.identity()));
                Map<Long, ContainerResponse> finalContMap = contMap;
                truckDriverDetails.forEach(truckDriverDetail -> {
                    var details = jsonHelper.convertValue(truckDriverDetail, TruckDriverDetailsResponse.class);
                    details.setShipmentNumber(map.get(truckDriverDetail.getShipmentId()));
                    if(details.getContainerId() != null && finalContMap.containsKey(details.getContainerId())) {
                        details.setContainerNumber(finalContMap.get(details.getContainerId()).getContainerNumber());
                    }
                    truckingInfo.add(details);
                });
            }
        }
        consolidationDetailsResponse.setTruckDriverDetails(truckingInfo);
    }
    private void calculationsOnRetrieve(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse) {
        try {
            consolidationDetailsResponse.setContainerSummary(containerService.calculateContainerSummary(consolidationDetails.getContainersList(), consolidationDetails.getTransportMode(), consolidationDetails.getContainerCategory()));
            consolidationDetailsResponse.setPackSummary(packingService.calculatePackSummary(consolidationDetails.getPackingList(), consolidationDetails.getTransportMode(), consolidationDetails.getContainerCategory(), new ShipmentMeasurementDetailsDto()));
            calculateChargeable(CommonRequestModel.buildRequest(jsonHelper.convertValue(consolidationDetailsResponse, ConsoleCalculationsRequest.class)));
            calculateDescOfGoodsAndHandlingInfo(consolidationDetails, consolidationDetailsResponse, false);
        }
        catch (Exception e) {
            log.error("Error in calculations while creating console response payload");
        }
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<MasterListRequest> listRequests = new ArrayList<>(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName() ));
        if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
            listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() ));

        if(masterDataResponse != null) {
            if (!Objects.isNull(consolidationDetailsResponse.getAllocations()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsResponse.getAllocations(), Allocations.class, fieldNameKeyMap, Allocations.class.getSimpleName() ));
            if (!Objects.isNull(consolidationDetailsResponse.getAchievedQuantities()))
                listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(consolidationDetailsResponse.getAchievedQuantities(), AchievedQuantities.class, fieldNameKeyMap, AchievedQuantities.class.getSimpleName() ));
            if(!Objects.isNull(consolidationDetailsResponse.getRoutingsList()))
                consolidationDetailsResponse.getRoutingsList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(consolidationDetailsResponse.getPackingList()))
                consolidationDetailsResponse.getPackingList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(consolidationDetailsResponse.getReferenceNumbersList()))
                consolidationDetailsResponse.getReferenceNumbersList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, ReferenceNumbers.class, fieldNameKeyMap, ReferenceNumbers.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(consolidationDetailsResponse.getContainersList()))
                consolidationDetailsResponse.getContainersList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId())));
        }

        MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
        masterListRequestV2.setMasterListRequests(listRequests);
        masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));

        Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
        masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST);

        if(masterDataResponse == null) {
            consolidationDetailsResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.MASTER_LIST));
            if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
                consolidationDetailsResponse.getCarrierDetails().setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.MASTER_LIST) );
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllUnlocationDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> locationCodes = new ArrayList<>(masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName() ));
        if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
            locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() )));

        if(masterDataResponse != null) {
            if(!Objects.isNull(consolidationDetailsResponse.getContainersList()))
                consolidationDetailsResponse.getContainersList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(consolidationDetailsResponse.getRoutingsList()))
                consolidationDetailsResponse.getRoutingsList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId())));
            if (!Objects.isNull(consolidationDetailsResponse.getArrivalDetails()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsResponse.getArrivalDetails(), ArrivalDepartureDetails.class, fieldNameKeyMap, ArrivalDepartureDetails.class.getSimpleName() +  " Arrival")));
            if (!Objects.isNull(consolidationDetailsResponse.getDepartureDetails()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(consolidationDetailsResponse.getDepartureDetails(), ArrivalDepartureDetails.class, fieldNameKeyMap, ArrivalDepartureDetails.class.getSimpleName() +  " Departure")));
        }

        Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
        masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS);

        if(masterDataResponse == null) {
            consolidationDetailsResponse.setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS) );
            if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
                consolidationDetailsResponse.getCarrierDetails().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCarrierDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> carrierList = new ArrayList<>();
        if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
            carrierList = new ArrayList<>(masterDataUtils.createInBulkCarriersRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName()));

        if(masterDataResponse != null) {
            if(!Objects.isNull(consolidationDetailsResponse.getRoutingsList())) {
                List<String> finalCarrierList = carrierList;
                consolidationDetailsResponse.getRoutingsList().forEach(r -> finalCarrierList.addAll(masterDataUtils.createInBulkCarriersRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId())));
                carrierList = finalCarrierList;
            }
        }

        Map v1Data = masterDataUtils.fetchInBulkCarriers(carrierList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER);

        if(masterDataResponse == null) {
            consolidationDetailsResponse.getCarrierDetails().setCarrierMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.CARRIER));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CARRIER, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCurrencyDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> currencyList = new ArrayList<>(masterDataUtils.createInBulkCurrencyRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName()));

        Map v1Data = masterDataUtils.fetchInCurrencyList(currencyList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.CURRENCIES);

        if(masterDataResponse == null) {
            consolidationDetailsResponse.setCurrenciesMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.CURRENCIES));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CURRENCIES, masterDataResponse);
        }


        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCommodityTypesInSingleCall(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> commodityTypes = new HashSet<>();
        if (!Objects.isNull(consolidationDetailsResponse.getContainersList()))
            consolidationDetailsResponse.getContainersList().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId() )));

        if(masterDataResponse != null) {
            if (!Objects.isNull(consolidationDetailsResponse.getPackingList()))
                consolidationDetailsResponse.getPackingList().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId() )));
        }

        Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY);

        if(masterDataResponse == null) {
            if (!Objects.isNull(consolidationDetailsResponse.getContainersList()))
                consolidationDetailsResponse.getContainersList().forEach(r -> r.setCommodityTypeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.COMMODITY)));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COMMODITY, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllTenantDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> tenantIdList = new ArrayList<>(masterDataUtils.createInBulkTenantsRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName()));

        Map v1Data = masterDataUtils.fetchInTenantsList(tenantIdList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.TENANTS);

        if(masterDataResponse == null) {
            consolidationDetailsResponse.setTenantIdsData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.TENANTS));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.TENANTS, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllWarehouseDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> wareHouseTypes = new HashSet<>(masterDataUtils.createInBulkWareHouseRequest(consolidationDetailsResponse, ConsolidationDetails.class, fieldNameKeyMap, ConsolidationDetails.class.getSimpleName()));

        Map v1Data = masterDataUtils.fetchInWareHousesList(wareHouseTypes.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.WAREHOUSES);

        if(masterDataResponse == null) {
            consolidationDetailsResponse.setTextData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ConsolidationDetails.class.getSimpleName()), CacheConstants.WAREHOUSES));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.WAREHOUSES, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllVesselDataInSingleCall(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> vesselList = new ArrayList<>();
        if (!Objects.isNull(consolidationDetailsResponse.getCarrierDetails()))
            vesselList.addAll((masterDataUtils.createInBulkVesselsRequest(consolidationDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() )));
        if (!Objects.isNull(consolidationDetailsResponse.getRoutingsList()))
            consolidationDetailsResponse.getRoutingsList().forEach(r -> vesselList.addAll(masterDataUtils.createInBulkVesselsRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId() )));

        Map v1Data = masterDataUtils.fetchInBulkVessels(vesselList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.VESSELS);

        if(masterDataResponse == null) {
            consolidationDetailsResponse.getCarrierDetails().setVesselsMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.VESSELS));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.VESSELS, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(Arrays.asList()));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllDGSubstanceDataInSingleCall (ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> dgSubstanceIdList = new HashSet<>();
        if (!Objects.isNull(consolidationDetailsResponse.getPackingList()))
            consolidationDetailsResponse.getPackingList().forEach(r -> dgSubstanceIdList.addAll(masterDataUtils.createInBulkDGSubstanceRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId() )));

        Map v1Data = masterDataUtils.fetchInDGSubstanceList(dgSubstanceIdList.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.DG_SUBSTANCES);

        if(masterDataResponse == null) { }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.DG_SUBSTANCES, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllContainerTypesInSingleCall(ConsolidationDetails consolidationDetails, ConsolidationDetailsResponse consolidationDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> containerTypes = new ArrayList<>();
        if (!Objects.isNull(consolidationDetailsResponse.getContainersList()))
            consolidationDetailsResponse.getContainersList().forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId() )));

        Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
        masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE);

        if(masterDataResponse == null) {
            if (!Objects.isNull(consolidationDetailsResponse.getContainersList()))
                consolidationDetailsResponse.getContainersList().forEach(r -> r.setContainerCodeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.CONTAINER_TYPE)));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CONTAINER_TYPE, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private List<Containers> mergeContainers(List<Containers> containersList, ShipmentSettingsDetails shipmentSettingsDetails) throws RunnerException{
        try {
            List<Containers> response = new ArrayList<>();
            HashMap<String, List<Containers>> map = new HashMap<>();
            for(Containers container: containersList) {
                if(IsStringNullOrEmpty(container.getContainerCode()) || IsStringNullOrEmpty(container.getContainerNumber()) || IsStringNullOrEmpty(container.getHblDeliveryMode())) {
                    response.add(container);
                    continue;
                }
                String key = container.getContainerCode() + container.getContainerNumber() + container.getHblDeliveryMode();
                if(map.containsKey(key)) {
                    List<Containers> temp = map.get(key);
                    temp.add(container);
                    map.put(key, temp);
                }
                else {
                    List<Containers> containersList1 = new ArrayList<>();
                    containersList1.add(container);
                    map.put(key, containersList1);
                }
            }
            if(map.size() > 0) {
                for (Map.Entry<String, List<Containers>> entry : map.entrySet()) {
                    List<Containers> value = entry.getValue();
                    if(value.size() == 1) {
                        response.add(value.get(0));
                    }
                    else if(entry.getValue().size() > 0){
                        Containers finalContainer = value.get(0);
                        finalContainer.setId(null);
                        finalContainer.setGuid(null);
                        for (int i=1; i < value.size(); i++) {
                            finalContainer = convertIntoMergedConts(finalContainer, value.get(i), shipmentSettingsDetails);
                        }
                        response.add(finalContainer);
                    }
                }
            }
            return response;
        } catch (Exception e) {
            log.error("Error while merging consol containers");
            throw new RunnerException(e.getMessage());
        }
    }

    private Containers convertIntoMergedConts(Containers finalContainer, Containers container, ShipmentSettingsDetails shipmentSettingsDetails) throws RunnerException{
        try {
            finalContainer.setDescriptionOfGoods(mergeTextField(finalContainer.getDescriptionOfGoods(), container.getDescriptionOfGoods()));
//        finalContainer.setHa(mergeTextField(finalContainer.getDescriptionOfGoods(), container.getDescriptionOfGoods())); TODO- missing handling info in containers
            if(finalContainer.getNoOfPackages() != null && container.getNoOfPackages() != null)
                finalContainer.setNoOfPackages(finalContainer.getNoOfPackages() + container.getNoOfPackages());
            else if(container.getNoOfPackages() != null)
                finalContainer.setNoOfPackages(container.getNoOfPackages());
            if(!IsStringNullOrEmpty(finalContainer.getPacks()) && !IsStringNullOrEmpty(container.getPacks()))
                finalContainer.setPacks(String.valueOf(new BigDecimal(finalContainer.getPacks()).add(new BigDecimal(container.getPacks()))));
            else if(!IsStringNullOrEmpty(container.getPacks())) {
                finalContainer.setPacks(container.getPacks());
            }
            finalContainer.setMarksNums(mergeTextField(finalContainer.getMarksNums(), container.getMarksNums()));
            finalContainer.setCustomsReleaseCode(mergeTextField(finalContainer.getCustomsReleaseCode(), container.getCustomsReleaseCode()));
            finalContainer.setPacrNumber(mergeTextField(finalContainer.getPacrNumber(), container.getPacrNumber()));
            finalContainer.setContainerComments(mergeTextField(finalContainer.getContainerComments(), container.getContainerComments()));
            if(!IsStringNullOrEmpty(finalContainer.getPacksType()) && !IsStringNullOrEmpty(container.getPacksType()) && !container.getPacksType().equals(finalContainer.getPacksType()))
                finalContainer.setPacksType(Constants.MPK);
            // Net Weight field
            if(finalContainer.getNetWeight() != null && container.getNetWeight() != null && finalContainer.getNetWeightUnit() != null && container.getNetWeightUnit() != null) {
                if(finalContainer.getNetWeightUnit().equals(container.getNetWeightUnit()))
                    finalContainer.setNetWeight(finalContainer.getNetWeight().add(container.getNetWeight()));
                else {
                    finalContainer.setNetWeight( new BigDecimal(convertUnit(Constants.MASS, finalContainer.getNetWeight(), finalContainer.getNetWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() +
                                    convertUnit(Constants.MASS, container.getNetWeight(), container.getNetWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() ));
                    finalContainer.setNetWeightUnit(shipmentSettingsDetails.getWeightChargeableUnit());
                }
            }
            else if(container.getNetWeight() != null){
                finalContainer.setNetWeightUnit(container.getNetWeightUnit());
                finalContainer.setNetWeight(container.getNetWeight());
            }
            // Gross Weight field
            if(finalContainer.getGrossWeight() != null && container.getGrossWeight() != null && finalContainer.getGrossWeightUnit() != null && container.getGrossWeightUnit() != null) {
                if(finalContainer.getGrossWeightUnit().equals(container.getGrossWeightUnit()))
                    finalContainer.setGrossWeight(finalContainer.getGrossWeight().add(container.getGrossWeight()));
                else {
                    finalContainer.setGrossWeight( new BigDecimal(convertUnit(Constants.MASS, finalContainer.getGrossWeight(), finalContainer.getGrossWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() +
                            convertUnit(Constants.MASS, container.getGrossWeight(), container.getGrossWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() ));
                    finalContainer.setGrossWeightUnit(shipmentSettingsDetails.getWeightChargeableUnit());
                }
            }
            else if(container.getGrossWeight() != null){
                finalContainer.setGrossWeightUnit(container.getGrossWeightUnit());
                finalContainer.setGrossWeight(container.getGrossWeight());
            }
            // Tare Weight field
            if(finalContainer.getTareWeight() != null && container.getTareWeight() != null && finalContainer.getTareWeightUnit() != null && container.getTareWeightUnit() != null) {
                if(finalContainer.getTareWeightUnit().equals(container.getTareWeightUnit()))
                    finalContainer.setTareWeight(finalContainer.getTareWeight().add(container.getTareWeight()));
                else {
                    finalContainer.setTareWeight( new BigDecimal(convertUnit(Constants.MASS, finalContainer.getTareWeight(), finalContainer.getTareWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() +
                            convertUnit(Constants.MASS, container.getTareWeight(), container.getTareWeightUnit(), shipmentSettingsDetails.getWeightChargeableUnit()).doubleValue() ));
                    finalContainer.setTareWeightUnit(shipmentSettingsDetails.getWeightChargeableUnit());
                }
            }
            else if(container.getTareWeight() != null){
                finalContainer.setTareWeightUnit(container.getTareWeightUnit());
                finalContainer.setTareWeight(container.getTareWeight());
            }
            // Gross Volume field
            if(finalContainer.getGrossVolume() != null && container.getGrossVolume() != null && finalContainer.getGrossVolumeUnit() != null && container.getGrossVolumeUnit() != null) {
                if(finalContainer.getGrossVolumeUnit().equals(container.getGrossVolumeUnit()))
                    finalContainer.setGrossVolume(finalContainer.getGrossVolume().add(container.getGrossVolume()));
                else {
                    finalContainer.setGrossVolume( new BigDecimal(convertUnit(Constants.VOLUME, finalContainer.getGrossVolume(), finalContainer.getGrossVolumeUnit(), shipmentSettingsDetails.getVolumeChargeableUnit()).doubleValue() +
                            convertUnit(Constants.VOLUME, container.getGrossVolume(), container.getGrossVolumeUnit(), shipmentSettingsDetails.getVolumeChargeableUnit()).doubleValue() ));
                    finalContainer.setGrossVolumeUnit(shipmentSettingsDetails.getVolumeChargeableUnit());
                }
            }
            else if(container.getGrossVolume() != null){
                finalContainer.setGrossVolumeUnit(container.getGrossVolumeUnit());
                finalContainer.setGrossVolume(container.getGrossVolume());
            }
            // Measurement field
            if(finalContainer.getMeasurement() != null && container.getMeasurement() != null && finalContainer.getMeasurementUnit() != null && container.getMeasurementUnit() != null) {
                if(finalContainer.getMeasurementUnit().equals(container.getMeasurementUnit()))
                    finalContainer.setMeasurement(finalContainer.getMeasurement().add(container.getMeasurement()));
                else {
                    finalContainer.setMeasurement( new BigDecimal(convertUnit(Constants.LENGTH, finalContainer.getMeasurement(), finalContainer.getMeasurementUnit(), shipmentSettingsDetails.getMeasurementChargeableUnit()).doubleValue() +
                            convertUnit(Constants.LENGTH, container.getMeasurement(), container.getMeasurementUnit(), shipmentSettingsDetails.getMeasurementChargeableUnit()).doubleValue() ));
                    finalContainer.setMeasurementUnit(shipmentSettingsDetails.getMeasurementChargeableUnit());
                }
            }
            else if(container.getMeasurement() != null){
                finalContainer.setMeasurementUnit(container.getMeasurementUnit());
                finalContainer.setMeasurement(container.getMeasurement());
            }
            return finalContainer;
        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    private String mergeTextField(String a, String b) {
        if(!IsStringNullOrEmpty(b) && !IsStringNullOrEmpty(b))
            return a + "\n" + b;
        if(!IsStringNullOrEmpty(b))
            return b;
        if(!IsStringNullOrEmpty(a))
            return a;
        return null;
    }

    public ResponseEntity<IRunnerResponse> toggleLock(CommonRequestModel commonRequestModel) throws RunnerException {
        CommonGetRequest commonGetRequest = (CommonGetRequest) commonRequestModel.getData();
        Long id = commonGetRequest.getId();
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(id).get();
        String lockingUser = consolidationDetails.getLockedBy();
        String currentUser = userContext.getUser().getUsername();

        if (consolidationDetails.getIsLocked() != null && consolidationDetails.getIsLocked()) {
            if (lockingUser != null && (Objects.equals(lockingUser, currentUser) ||
                    (!Objects.isNull(PermissionsContext.getPermissions(PermissionConstants.tenantSuperAdmin)) && !PermissionsContext.getPermissions(PermissionConstants.tenantSuperAdmin).isEmpty()) ))
                consolidationDetails.setIsLocked(false);
            else
                throw new RunnerException(String.format(ErrorConstants.LOCK_UNLOCK_ERROR, Constants.Consolidation, lockingUser));
        } else {
            consolidationDetails.setIsLocked(true);
            consolidationDetails.setLockedBy(currentUser);
        }
        consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false);
        consolidationSync.syncLockStatus(consolidationDetails);
        pushShipmentDataToDependentService(consolidationDetails, false);

        return ResponseHelper.buildSuccessResponse();
    }

//    private <T extends IRunnerResponse> List<T> getResponse(CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntity) throws ExecutionException, InterruptedException {
//        var runnerListResponse = (RunnerListResponse<T>) responseEntity.get().getBody();
//        return (List<T>) runnerListResponse.getData();
//    }
//
//    private <T extends IRunnerResponse> List<T> getResponse(ResponseEntity<?> responseEntity) throws ExecutionException, InterruptedException {
//        var runnerListResponse = (RunnerListResponse<T>) responseEntity.getBody();
//        return (List<T>) runnerListResponse.getData();
//    }
//
//    private <T extends IRunnerResponse> T getResponseEntity(ResponseEntity<?> responseEntity) throws ExecutionException, InterruptedException {
//        var runnerResponse = (RunnerResponse<T>) responseEntity.getBody();
//        return (T) runnerResponse.getData();
//    }
//
//    private Containers convertRequestToEntity(ContainerRequest request) {
//        return jsonHelper.convertValue(request, Containers.class);
//    }

    @Transactional
    public ResponseEntity<IRunnerResponse> completeV1ConsolidationCreateAndUpdate(CommonRequestModel commonRequestModel, boolean dataMigration, String createdBy, LocalDateTime createdDate) throws RunnerException {
        ConsolidationDetailsRequest consolidationDetailsRequest = (ConsolidationDetailsRequest) commonRequestModel.getData();

        List<PackingRequest> packingRequestList = consolidationDetailsRequest.getPackingList();
        List<ContainerRequest> containerRequestList = consolidationDetailsRequest.getContainersList();
        List<EventsRequest> eventsRequestList = consolidationDetailsRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = consolidationDetailsRequest.getFileRepoList();
        List<JobRequest> jobRequestList = consolidationDetailsRequest.getJobsList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = consolidationDetailsRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = consolidationDetailsRequest.getRoutingsList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = consolidationDetailsRequest.getTruckDriverDetails();
        List<PartiesRequest> consolidationAddresses = consolidationDetailsRequest.getConsolidationAddresses();
        CarrierDetailRequest carrierDetailRequest = consolidationDetailsRequest.getCarrierDetails();
        AchievedQuantitiesRequest achievedQuantitiesRequest = consolidationDetailsRequest.getAchievedQuantities();
        AllocationsRequest allocationsRequest = consolidationDetailsRequest.getAllocations();


        UUID guid = consolidationDetailsRequest.getGuid();
        Optional<ConsolidationDetails> oldEntity = Optional.empty();
        try {
            oldEntity = consolidationDetailsDao.findByGuid(guid);
        }
        catch (Exception e){
        }

        List<ShipmentDetails> tempShipIds = new ArrayList<>();

        List<Long> newShipList = new ArrayList<>();
        List<ShipmentRequest> shipmentRequests = consolidationDetailsRequest.getShipmentsList();
        if(shipmentRequests != null && !shipmentRequests.isEmpty()) {
            for(ShipmentRequest shipmentRequest : shipmentRequests) {
                Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(shipmentRequest.getGuid());
                if(shipmentDetails.isPresent() && shipmentDetails.get().getId() != null) {
                    ShipmentDetails shipmentDetails1 = new ShipmentDetails();
                    shipmentDetails1.setId(shipmentDetails.get().getId());
                    tempShipIds.add(shipmentDetails1);
                    newShipList.add(shipmentDetails.get().getId());
                }
            }
        }


        try {
            Long id = null;
            ConsolidationDetails oldConsolidation = null;
            boolean isCreate = true;
            if(oldEntity.isPresent()) {
                oldConsolidation = oldEntity.get();
                id = oldEntity.get().getId();
                List<Long> oldShipList = oldConsolidation.getShipmentsList().stream().map(e -> e.getId()).toList();
                oldShipList = oldShipList.stream().filter(item -> !newShipList.contains(item)).toList();
                detachShipmentsReverseSync(oldEntity.get().getId(), oldShipList);
                isCreate = false;
            }
            ConsolidationDetails entity = objectMapper.convertValue(consolidationDetailsRequest, ConsolidationDetails.class);
            entity.setShipmentsList(tempShipIds);
            entity.setId(id);

            if(id == null) {
                entity = consolidationDetailsDao.save(entity, true);
            } else {
                entity = consolidationDetailsDao.update(entity, true);
            }
            // Set id for linking child entitites
            id = entity.getId();

            consolidationDetailsDao.saveCreatedDateAndUser(id, createdBy, createdDate);

            if(containerRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, entity.getId(), "=");
                if(!entity.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && entity.getShipmentsList() != null && !entity.getShipmentsList().isEmpty()) {
                    for (ShipmentDetails shipmentDetails : entity.getShipmentsList()) {
                        listCommonRequest = orCriteria(Constants.SHIPMENTS_LIST, shipmentDetails.getId(), "CONTAINS", listCommonRequest);
                    }
                }
                Pair<Specification<Containers>, Pageable> containerPair = fetchData(listCommonRequest, Containers.class);
                Page<Containers> oldContainers = containerDao.findAll(containerPair.getLeft(), containerPair.getRight());
                List<Containers> updatedContainers = containerDao.updateEntityFromConsolidationV1(commonUtils.convertToEntityList(containerRequestList, Containers.class), id, oldContainers.stream().toList());
                entity.setContainersList(updatedContainers);
            }
            if (packingRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, entity.getId(), "=");
                if(entity.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && entity.getShipmentsList() != null && !entity.getShipmentsList().isEmpty()) {
                    for (ShipmentDetails shipmentDetails : entity.getShipmentsList()) {
                        listCommonRequest = orCriteria(Constants.SHIPMENT_ID, shipmentDetails.getId(), "=", listCommonRequest);
                    }
                }
                Pair<Specification<Packing>, Pageable> packingPair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> oldPackings = packingDao.findAll(packingPair.getLeft(), packingPair.getRight());
                List<Packing> updatedPackings = packingDao.updateEntityFromConsole(commonUtils.convertToEntityList(packingRequestList, Packing.class), id, oldPackings.stream().toList());
                entity.setPackingList(updatedPackings);
            }
            if (eventsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.CONSOLIDATION);
                Pair<Specification<Events>, Pageable> pair = fetchData(listCommonRequest, Events.class);
                Page<Events> oldEvents = eventDao.findAll(pair.getLeft(), pair.getRight());
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(eventsRequestList, Events.class), id, Constants.CONSOLIDATION, oldEvents.stream().toList());
                entity.setEventsList(updatedEvents);
            }
            if (referenceNumbersRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, entity.getId(), "=");
                Pair<Specification<ReferenceNumbers>, Pageable> pair = fetchData(listCommonRequest, ReferenceNumbers.class);
                Page<ReferenceNumbers> oldReferenceNumbers = referenceNumbersDao.findAll(pair.getLeft(), pair.getRight());
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromConsole(commonUtils.convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id, oldReferenceNumbers.stream().toList());
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (routingsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, entity.getId(), "=");
                Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
                Page<Routings> oldRoutings = routingsDao.findAll(pair.getLeft(), pair.getRight());
                List<Routings> updatedRoutings = routingsDao.updateEntityFromConsole(commonUtils.convertToEntityList(routingsRequestList, Routings.class), id, oldRoutings.stream().toList());
                entity.setRoutingsList(updatedRoutings);
            }
            if (consolidationAddresses != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.CONSOLIDATION_ADDRESSES);
                Pair<Specification<Parties>, Pageable> pair = fetchData(listCommonRequest, Parties.class);
                Page<Parties> oldParties = partiesDao.findAll(pair.getLeft(), pair.getRight());
                List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(consolidationAddresses, Parties.class), id, Constants.CONSOLIDATION_ADDRESSES, oldParties.stream().toList());
                entity.setConsolidationAddresses(updatedParties);
            }
            if(!dataMigration)
                pushShipmentDataToDependentService(entity, isCreate);
            ConsolidationDetailsResponse response = jsonHelper.convertValue(entity, ConsolidationDetailsResponse.class);

            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RunnerException(e.getMessage());
        }
    }

    private void makeHeadersInSheet(Sheet sheet, Workbook workbook) {
        Row headerRow = sheet.createRow(0);
        List<String> consolidationHeader = ConsolidationConstants.CONSOLIDATION_HEADER;

        CellStyle boldStyle = workbook.createCellStyle();
        Font boldFont = workbook.createFont();
        boldFont.setBold(true);
        boldStyle.setFont(boldFont);

        for (int i = 0; i < consolidationHeader.size(); i++) {
            Cell cell = headerRow.createCell(i);
            cell.setCellValue(consolidationHeader.get(i));
            cell.setCellStyle(boldStyle);
        }
    }

    @Override
    public void exportExcel(HttpServletResponse response, CommonRequestModel commonRequestModel) throws IOException, IllegalAccessException {
        String responseMsg;

        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        if (request == null) {
            log.error(CONSOLIDATION_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
        }
        Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
        Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
        List<IRunnerResponse> consoleResponse = convertEntityListToDtoList(consolidationDetailsPage.getContent());
        log.info("Consolidation list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        Map<String, Integer> headerMap = new HashMap<>();
        for (int i = 0; i < ConsolidationConstants.CONSOLIDATION_HEADER.size(); i++) {
            headerMap.put(ConsolidationConstants.CONSOLIDATION_HEADER.get(i), i);
        }
        try(Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("ConsolidationList");
            makeHeadersInSheet(sheet, workbook);

            for (int i = 0; i < consoleResponse.size(); i++) {
                Row itemRow = sheet.createRow(i + 1);
                ConsolidationListResponse consol = (ConsolidationListResponse) consoleResponse.get(i);
                LocalTimeZoneHelper.transformTimeZone(consol);
                itemRow.createCell(headerMap.get("Consolidation Type")).setCellValue(consol.getConsolidationType() != null ? consol.getConsolidationType() : "");
                itemRow.createCell(headerMap.get("Consolidation Number")).setCellValue(consol.getConsolidationNumber() != null ? consol.getConsolidationNumber() : "");
                itemRow.createCell(headerMap.get("Transport Mode")).setCellValue(consol.getTransportMode() != null ? consol.getTransportMode() : "");
                itemRow.createCell(headerMap.get("Cargo Type")).setCellValue(consol.getShipmentType() != null ? consol.getShipmentType() : "");
                itemRow.createCell(headerMap.get("ETA")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getEta() != null ? consol.getCarrierDetails().getEta().toString() : "");
                itemRow.createCell(headerMap.get("ATA")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getAta() != null ? consol.getCarrierDetails().getAta().toString() : "");
                itemRow.createCell(headerMap.get("ETD")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getEtd() != null ? consol.getCarrierDetails().getEtd().toString() : "");
                itemRow.createCell(headerMap.get("ATD")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getAtd() != null ? consol.getCarrierDetails().getAtd().toString() : "");
                itemRow.createCell(headerMap.get("Domestic")).setCellValue(consol.getIsDomestic() != null ? consol.getIsDomestic().toString() : "");
                itemRow.createCell(headerMap.get("Created By")).setCellValue(consol.getCreatedBy() != null ? consol.getCreatedBy() : "");
                itemRow.createCell(headerMap.get("Voyage/Flight No")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getVoyage() != null ? consol.getCarrierDetails().getVoyage() : "");
                itemRow.createCell(headerMap.get("Payment Terms")).setCellValue(consol.getPayment() != null ? consol.getPayment() : "");
                itemRow.createCell(headerMap.get("Carrier")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getShippingLine() != null ? consol.getCarrierDetails().getShippingLine() : "");
                itemRow.createCell(headerMap.get("Cutoff Date")).setCellValue(consol.getBookingCutoff() != null ? consol.getBookingCutoff().toString() : "");
                itemRow.createCell(headerMap.get("HBL / HAWB")).setCellValue(consol.getHouseBills() != null && !consol.getHouseBills().isEmpty() ? consol.getHouseBills().get(0) : "");
                itemRow.createCell(headerMap.get("Estimated Terminal Cutoff")).setCellValue(consol.getEstimatedTerminalCutoff() != null ? consol.getEstimatedTerminalCutoff().toString() : "");
                itemRow.createCell(headerMap.get("Terminal Cutoff")).setCellValue(consol.getTerminalCutoff() != null ? consol.getTerminalCutoff().toString() : "");
                itemRow.createCell(headerMap.get("Booking Cutoff")).setCellValue(consol.getBookingCutoff() != null ? consol.getBookingCutoff().toString() : "");
                itemRow.createCell(headerMap.get("Shipping Instruction Cutoff")).setCellValue(consol.getShipInstructionCutoff() != null ? consol.getShipInstructionCutoff().toString() : "");
                itemRow.createCell(headerMap.get("Hazardous Booking Cutoff")).setCellValue(consol.getHazardousBookingCutoff() != null ? consol.getHazardousBookingCutoff().toString() : "");
                itemRow.createCell(headerMap.get("VGM Cutoff")).setCellValue(consol.getVerifiedGrossMassCutoff() != null ? consol.getVerifiedGrossMassCutoff().toString() : "");
                itemRow.createCell(headerMap.get("Reefer Cutoff")).setCellValue(consol.getReeferCutoff() != null ? consol.getReeferCutoff().toString() : "");
                itemRow.createCell(headerMap.get("Booking Type")).setCellValue(consol.getBookingType() != null ? consol.getBookingType() : "");
                itemRow.createCell(headerMap.get("Reference Number")).setCellValue(consol.getReferenceNumber() != null ? consol.getReferenceNumber() : "");
                itemRow.createCell(headerMap.get("Carrier Booking Status")).setCellValue(consol.getBookingStatus() != null ? consol.getBookingStatus() : "");
                itemRow.createCell(headerMap.get("Carrier Booking Number")).setCellValue(consol.getBookingNumber() != null ? consol.getBookingNumber() : "");
                itemRow.createCell(headerMap.get("Container Count")).setCellValue(consol.getContainerCount() != null ? consol.getContainerCount().toString() : "");
                itemRow.createCell(headerMap.get("POL")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? consol.getCarrierDetails().getUnlocationData().get("originPort_name") : "");
                itemRow.createCell(headerMap.get("POD")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? consol.getCarrierDetails().getUnlocationData().get("destinationPort_name") : "");
                itemRow.createCell(headerMap.get("MBL / MAWB")).setCellValue(consol.getMawb() != null ? consol.getMawb() : "");
                itemRow.createCell(headerMap.get("POL Code")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? String.valueOf(consol.getCarrierDetails().getUnlocationData().get("originPort_code")) : "");
                itemRow.createCell(headerMap.get("POD Code")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? String.valueOf(consol.getCarrierDetails().getUnlocationData().get("destinationPort_code")) : "");
                itemRow.createCell(headerMap.get("Origin Code")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? String.valueOf(consol.getCarrierDetails().getUnlocationData().get("origin_code")) : "");
                itemRow.createCell(headerMap.get("Destination Code")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? String.valueOf(consol.getCarrierDetails().getUnlocationData().get("destination_code")) : "");
                itemRow.createCell(headerMap.get("Origin")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? String.valueOf(consol.getCarrierDetails().getUnlocationData().get("origin_name")) : "");
                itemRow.createCell(headerMap.get("Destination")).setCellValue(consol.getCarrierDetails() != null && consol.getCarrierDetails().getUnlocationData() != null ? String.valueOf(consol.getCarrierDetails().getUnlocationData().get("destination_name")) : "");
            }

            LocalDateTime currentTime = LocalDateTime.now();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_HH_MM_SS_FORMAT);
            String timestamp = currentTime.format(formatter);
            String filenameWithTimestamp = "Consolidations_" + timestamp + Constants.XLSX;

            response.setContentType(Constants.CONTENT_TYPE_FOR_EXCEL);
            response.setHeader("Content-Disposition", "attachment; filename=" + filenameWithTimestamp);

            try (OutputStream outputStream = response.getOutputStream()) {
                workbook.write(outputStream);
            }
        }

    }

    private void beforeSave(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, Boolean isCreate) throws Exception {
        if (Objects.isNull(consolidationDetails.getSourceTenantId()))
            consolidationDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));
        log.info("Executing consolidation before save");
        // assign consolidation bol to mawb field as well
        if (consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
            consolidationDetails.setMawb(consolidationDetails.getBol());
        }
        if(!isCreate){
            calculateAchievedValues(consolidationDetails, new ShipmentGridChangeResponse(), oldEntity.getShipmentsList());
            updateLinkedShipmentData(consolidationDetails, oldEntity, false);
        }
        if(consolidationDetails.getCarrierDetails() != null) {
            if (consolidationDetails.getTransportMode() != null && consolidationDetails.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) {
                consolidationDetails.getCarrierDetails().setVoyage(null);
            } else {
                consolidationDetails.getCarrierDetails().setFlightNumber(null);
            }
        }
        if(consolidationDetails.getReceivingBranch() != null && consolidationDetails.getReceivingBranch() == 0)
            consolidationDetails.setReceivingBranch(null);
        if(consolidationDetails.getDocumentationPartner() != null && consolidationDetails.getDocumentationPartner() == 0)
            consolidationDetails.setDocumentationPartner(null);
        if(consolidationDetails.getTriangulationPartner() != null && consolidationDetails.getTriangulationPartner() == 0)
            consolidationDetails.setTriangulationPartner(null);

        if(checkDisableFetchConditionForAwb(consolidationDetails, oldEntity, ShipmentSettingsDetailsContext.getCurrentTenantSettings())) {
            List<Awb> awbs = awbDao.findByConsolidationId(consolidationDetails.getId());
            if(!awbs.isEmpty()) {
                Awb awb = awbs.get(0);
                awb.getAwbGoodsDescriptionInfo().forEach(x -> {
                    x.setDisableFetchRates(false);
                    x.setEnableFetchRatesWarning(true);
                });
                awbDao.save(awb);
            }
        }
    }

    private boolean checkDisableFetchConditionForAwb(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity,ShipmentSettingsDetails shipmentSettingsDetails){
        if(oldEntity == null)
            return false;
        if(!Boolean.TRUE.equals(shipmentSettingsDetails.getIataTactFlag())){
            return false;
        }
        if(!Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR))
            return false;
        return !Objects.equals(consolidationDetails.getCarrierDetails().getOriginPort(), oldEntity.getCarrierDetails().getOriginPort()) || !Objects.equals(consolidationDetails.getCarrierDetails().getDestinationPort(), oldEntity.getCarrierDetails().getDestinationPort())
                || !Objects.equals(consolidationDetails.getCarrierDetails().getShippingLine(), oldEntity.getCarrierDetails().getShippingLine());
    }

    private void afterSave(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, ConsolidationDetailsRequest consolidationDetailsRequest, Boolean isCreate, ShipmentSettingsDetails shipmentSettingsDetails, Boolean isFromBooking) throws RunnerException{
        List<PackingRequest> packingRequestList = consolidationDetailsRequest.getPackingList();
        List<ContainerRequest> containerRequestList = consolidationDetailsRequest.getContainersList();
        List<EventsRequest> eventsRequestList = consolidationDetailsRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = consolidationDetailsRequest.getFileRepoList();
        List<JobRequest> jobRequestList = consolidationDetailsRequest.getJobsList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = consolidationDetailsRequest.getReferenceNumbersList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = consolidationDetailsRequest.getTruckDriverDetails();
        List<RoutingsRequest> routingsRequestList = consolidationDetailsRequest.getRoutingsList();
        List<PartiesRequest> consolidationAddressRequest = consolidationDetailsRequest.getConsolidationAddresses();

        Long id = consolidationDetails.getId();

        if(isCreate){
            if(shipmentSettingsDetails.getAutoEventCreate() != null && shipmentSettingsDetails.getAutoEventCreate()) {
                autoGenerateEvents(consolidationDetails);
            }
        }

        if(containerRequestList != null && (shipmentSettingsDetails.getMergeContainers() == null || !shipmentSettingsDetails.getMergeContainers())
            && (shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer())) {
            List<Containers> updatedContainers = containerDao.updateEntityFromShipmentConsole(commonUtils.convertToEntityList(containerRequestList, Containers.class, isFromBooking ? false : isCreate), id, (Long) null, true);
            consolidationDetails.setContainersList(updatedContainers);
        }
        if (packingRequestList != null) {
            List<Packing> updatedPackings = packingDao.updateEntityFromConsole(commonUtils.convertToEntityList(packingRequestList, Packing.class, isFromBooking ? false : isCreate), id);
            consolidationDetails.setPackingList(updatedPackings);
        }
        if (eventsRequestList != null) {
            List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(eventsRequestList, Events.class, isFromBooking ? false : isCreate), id, Constants.CONSOLIDATION);
            consolidationDetails.setEventsList(updatedEvents);
        }
        if (referenceNumbersRequestList != null) {
            List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromConsole(commonUtils.convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class, isFromBooking ? false : isCreate), id);
            consolidationDetails.setReferenceNumbersList(updatedReferenceNumbers);
        }
        if (truckDriverDetailsRequestList != null) {
            List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromConsole(commonUtils.convertToEntityList(truckDriverDetailsRequestList, TruckDriverDetails.class, isFromBooking ? false : isCreate), id);
//            consolidationDetails.setTruckDriverDetails(updatedTruckDriverDetails);
        }
        if (routingsRequestList != null) {
            List<Routings> updatedRoutings = routingsDao.updateEntityFromConsole(commonUtils.convertToEntityList(routingsRequestList, Routings.class, isFromBooking ? false : isCreate), id);
            consolidationDetails.setRoutingsList(updatedRoutings);
        }
        if (consolidationAddressRequest != null) {
            List<Parties> updatedFileRepos = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(consolidationAddressRequest, Parties.class, isFromBooking ? false : isCreate), id, Constants.CONSOLIDATION_ADDRESSES);
            consolidationDetails.setConsolidationAddresses(updatedFileRepos);
        }

        pushShipmentDataToDependentService(consolidationDetails, isCreate);
        try {
            if (!isFromBooking)
                consolidationSync.sync(consolidationDetails, StringUtility.convertToString(consolidationDetails.getGuid()), isFromBooking);
        } catch (Exception e){
            log.error("Error performing sync on consolidation entity, {}", e);
        }
        if (consolidationDetails.getShipmentsList() != null) {
            consolidationDetails.getShipmentsList().forEach(shipment -> {
                if (TenantSettingsDetailsContext.getCurrentTenantSettings().getP100Branch() != null && TenantSettingsDetailsContext.getCurrentTenantSettings().getP100Branch())
                    CompletableFuture.runAsync(masterDataUtils.withMdc(() -> bookingIntegrationsUtility.updateBookingInPlatform(shipment)), executorService);
            });
        }
    }

    @Override
    public void pushShipmentDataToDependentService(ConsolidationDetails consolidationDetails, boolean isCreate)
    {
        try {
            if(consolidationDetails.getTenantId() == null)
                consolidationDetails.setTenantId(TenantContext.getCurrentTenant());
            KafkaResponse kafkaResponse = producer.getKafkaResponse(consolidationDetails, isCreate);
            log.info("Producing consolidation data to kafka with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(kafkaResponse));
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, UUID.randomUUID().toString());
        }
        catch (Exception e) {
            log.error("Error Producing consolidation to kafka, error is due to " + e.getMessage());
        }
        try {
            if(trackingServiceAdapter.checkIfConsolContainersExist(consolidationDetails) || trackingServiceAdapter.checkIfAwbExists(consolidationDetails)) {
                UniversalTrackingPayload _utPayload = trackingServiceAdapter.mapConsoleDataToTrackingServiceData(consolidationDetails);
                List<UniversalTrackingPayload> trackingPayloads = new ArrayList<>();
                if(_utPayload != null) {
                    trackingPayloads.add(_utPayload);
                    var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                    log.info("Producing tracking service payload from consolidation with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonBody);
                    trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody, false);
                }
            }
            if(consolidationDetails != null) {
                var events = trackingServiceAdapter.getAllEvents(null,consolidationDetails, consolidationDetails.getReferenceNumber());
                var universalEventsPayload = trackingServiceAdapter.mapEventDetailsForTracking(consolidationDetails.getReferenceNumber(),Constants.CONSOLIDATION, consolidationDetails.getConsolidationNumber(), events);
                List<UniversalTrackingPayload.UniversalEventsPayload> trackingPayloads= new ArrayList<>();
                if(universalEventsPayload != null) {
                    trackingPayloads.add(universalEventsPayload);
                    var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                    log.info("Producing tracking service payload from consolidation with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonBody);
                    trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody, true);
                }
            }
        } catch (Exception e) {
            log.error(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getConsolFromShipment(Long shipmentId) {
        ConsolidationDetailsResponse consol;
        Optional<ShipmentDetails> shipmentRes = shipmentDao.findById(shipmentId);

        if (shipmentRes.isEmpty())
            throw new DataRetrievalFailureException("Failed to fetch the ShipmentId with id " + shipmentId);

        var shipment = modelMapper.map(shipmentRes.get(), ShipmentDetailsResponse.class);

        var additionalDetails = shipment.getAdditionalDetails();
        var shipmentCarrierDetails = shipment.getCarrierDetails();
        var tenantSettings = shipmentSettingsDao.findByTenantId(TenantContext.getCurrentTenant());
        if (tenantSettings.isEmpty())
            throw new DataRetrievalFailureException("Failed to fetch the shipment settings for tenant id " + TenantContext.getCurrentTenant());

        boolean isPayment = tenantSettings.get().getShipmentLite()
                && shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)
                && shipment.getDirection().equals(Constants.DIRECTION_EXP);

        boolean isMawb = tenantSettings.get().getShipmentLite()
                && shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)
                && shipment.getShipmentType().equals(Constants.SHIPMENT_TYPE_DRT);


        RoutingsResponse customRouting = RoutingsResponse.builder()
                .leg(1L)
                .pod(shipmentCarrierDetails != null ? shipmentCarrierDetails.getDestination() : null)
                .pol(shipmentCarrierDetails != null ? shipmentCarrierDetails.getOrigin() : null)
                .routingStatus(Constants.ROUTING_CFD)
                .mode(shipment.getTransportMode())
                .vesselName(shipmentCarrierDetails != null ? shipmentCarrierDetails.getVessel() : null)
                .voyage(shipmentCarrierDetails != null ? shipmentCarrierDetails.getVoyage() : null)
                .eta(shipmentCarrierDetails != null ? shipmentCarrierDetails.getEta() : null)
                .etd(shipmentCarrierDetails != null ? shipmentCarrierDetails.getEtd() : null)
                .build();


        String transportMode = shipment.getTransportMode() == null ? tenantSettings.get().getDefaultTransportMode(): shipment.getTransportMode();

        consol = ConsolidationDetailsResponse.builder()
                .consolidationType(Constants.SHIPMENT_TYPE_DRT)
                .transportMode(transportMode)
                .containerCategory(shipment.getShipmentType() == null ? tenantSettings.get().getDefaultContainerType() : shipment.getShipmentType())
                .declarationType(additionalDetails != null ? additionalDetails.getCustomDeclType() : null)
                .carrierDetails(CarrierDetailResponse.builder()
                        .origin(shipmentCarrierDetails != null ? shipmentCarrierDetails.getOriginPort() : null)
                        .destination(shipmentCarrierDetails != null ? shipmentCarrierDetails.getDestinationPort() : null)
                        .vessel(shipmentCarrierDetails != null ? shipmentCarrierDetails.getVessel() : null)
                        .originPort(shipmentCarrierDetails != null ? shipmentCarrierDetails.getOriginPort() : null)
                        .destinationPort(shipmentCarrierDetails != null ? shipmentCarrierDetails.getDestinationPort() : null)
                        .eta(shipmentCarrierDetails != null ? shipmentCarrierDetails.getEta() : null)
                        .etd(shipmentCarrierDetails != null ? shipmentCarrierDetails.getEtd() : null)
                        .ata(shipmentCarrierDetails != null ? shipmentCarrierDetails.getAta() : null)
                        .atd(shipmentCarrierDetails != null ? shipmentCarrierDetails.getAtd() : null)
                        .aircraftType(shipmentCarrierDetails != null ? shipmentCarrierDetails.getAircraftType() : null)
                        .flightNumber(shipmentCarrierDetails != null ? shipmentCarrierDetails.getFlightNumber() : null)
                        .shippingLine(shipmentCarrierDetails != null ? shipmentCarrierDetails.getShippingLine() : null) // carrier
                        .voyage(shipmentCarrierDetails != null ? shipmentCarrierDetails.getVoyage() : null)
                        .build())
                .departureDetails(ArrivalDepartureDetailsResponse.builder().firstForeignPort(shipmentCarrierDetails.getOriginPort())
                        .lastForeignPort(shipmentCarrierDetails.getDestinationPort()).build())
                .releaseType(additionalDetails != null ? additionalDetails.getReleaseType() : null)
                .original(additionalDetails != null ? additionalDetails.getOriginal() : null)
                .copy(additionalDetails != null ? additionalDetails.getCopy() : null)
                .allocations(AllocationsResponse.builder()
//                        .weight(shipment.getWeight()) // commented just like the v1 code
                        .weightUnit(shipment.getWeightUnit())
//                        .volume(shipment.getVolume())
                        .volumeUnit(shipment.getVolumeUnit())
//                        .chargable(shipment.getChargable())
                        .chargeableUnit(shipment.getChargeableUnit())
                        .build())
                .shipmentType(shipment.getDirection() == null ? null : shipment.getDirection())
                .igmFileDate(additionalDetails != null ? additionalDetails.getIGMFileDate() : null)
                .igmFileNo(additionalDetails != null ? additionalDetails.getIGMFileNo() : null)
                .smtpigmDate(additionalDetails != null ? additionalDetails.getSMTPIGMDate() : null)
                .smtpigmNumber(additionalDetails != null ? additionalDetails.getSMTPIGMNumber() : null)
                .igmInwardDate(additionalDetails != null ? additionalDetails.getIGMInwardDate() : null)
                .inwardDateAndTime(additionalDetails != null ? additionalDetails.getInwardDateAndTime() : null)
                .warehouseId(additionalDetails != null ? additionalDetails.getWarehouseId() : null)
                .bol(shipment.getMasterBill() != null && !shipment.getMasterBill().isEmpty() ? shipment.getMasterBill() : ((transportMode == null || !transportMode.equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) ? generateCustomBolNumber() : null))
                .referenceNumber(shipment.getBookingReference())
                .payment(isPayment ? shipment.getPaymentTerms() : null)
                .routingsList(List.of(customRouting))
                .mawb(isMawb ? shipment.getMasterBill() : null)
                .createdBy(UserContext.getUser().getUsername())
                .modeOfBooking(StringUtils.equals(transportMode, Constants.TRANSPORT_MODE_SEA) ? Constants.INTTRA : null)
                //.isLinked(true)
                .build();

        if(additionalDetails != null) {
            PartiesResponse parties;
            if(additionalDetails.getImportBroker() != null) {
                parties = jsonHelper.convertValue(additionalDetails.getImportBroker(), PartiesResponse.class);
                parties.setId(null);
                parties.setGuid(null);
                consol.setReceivingAgent(parties);
            }
            if(additionalDetails.getExportBroker() != null) {
                parties = jsonHelper.convertValue(additionalDetails.getExportBroker(), PartiesResponse.class);
                parties.setId(null);
                parties.setGuid(null);
                consol.setSendingAgent(parties);
            }
        }

        createConsolidationPayload(modelMapper.map(consol, ConsolidationDetails.class), consol);

        return ResponseHelper.buildSuccessResponse(consol);
    }
    @Override
    public ResponseEntity<IRunnerResponse> getAutoAttachConsolidationDetails(CommonRequestModel commonRequestModel){
        AutoAttachConsolidationRequest request = (AutoAttachConsolidationRequest) commonRequestModel.getData();
        AutoAttachConsolidationResponse response = new AutoAttachConsolidationResponse();

        List<Integer> itemTypeList = new ArrayList<>();
        itemTypeList.add(MasterDataType.CONSOLIDATION_CHECK_ORDER.getId());
        itemTypeList.add(MasterDataType.CONSOL_CHECK_ETD_ETD_THRESHOLD.getId());
        itemTypeList.add(MasterDataType.AUTO_ATTACH_TRANSPORT.getId());
        List<Object> masterDataCriteria = Arrays.asList(
                Arrays.asList(MasterDataConstants.ITEM_TYPE),
                "in",
                Arrays.asList(itemTypeList)
        );
        CommonV1ListRequest masterDataRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(masterDataCriteria).build();
        V1DataResponse masterDataResponse = v1Service.fetchMasterData(masterDataRequest);
        List<EntityTransferMasterLists> masterLists = jsonHelper.convertValueToList(masterDataResponse.entities, EntityTransferMasterLists.class);

        var applicableTransportModes = masterLists.stream()
                .filter(x -> x.getItemType() == (int)MasterDataType.AUTO_ATTACH_TRANSPORT.getId())
                .map(EntityTransferMasterLists::getItemDescription)
                .map(description -> description != null ? Arrays.asList(description.split(",")): null)
                .findFirst();
        List<String> applicableTransportModesList = applicableTransportModes.orElse(null);

        boolean isConditionSatisfied = false;
        ListCommonRequest consolListRequest = null;

        if(!Strings.isNullOrEmpty(request.getTransportMode()) && applicableTransportModesList != null &&
                applicableTransportModesList.contains(request.getTransportMode().toUpperCase())){

            consolListRequest = CommonUtils.andCriteria("transportMode", request.getTransportMode(), "=", consolListRequest);
            if(!Strings.isNullOrEmpty(request.getMasterBill())){
                consolListRequest = CommonUtils.andCriteria("bol", request.getMasterBill(), "=", consolListRequest);
                isConditionSatisfied = true;
                response.setFilteredDetailName("Master Bill");
            } else if(request.getEta() != null && request.getEtd() != null){
                var thresholdDetails = masterLists.stream()
                        .filter(x -> x.getItemType() == (int)MasterDataType.CONSOL_CHECK_ETD_ETD_THRESHOLD.getId())
                        .map(EntityTransferMasterLists::getItemDescription)
                        .findFirst();
                Long thresholdLimit = 0L;
                if(thresholdDetails.isPresent()){
                    thresholdLimit = Long.parseLong(thresholdDetails.get());
                }
                Long negativeThresholdLimit = thresholdLimit * -1;
                LocalDateTime eta = request.getEta();
                LocalDateTime etd = request.getEtd();

                var thresholdETAFrom = eta.plusDays(negativeThresholdLimit);
                var thresholdETATo = eta.plusDays(thresholdLimit + 1).plusSeconds(-1);
                var thresholdETDFrom = etd.plusDays(negativeThresholdLimit);
                var thresholdETDTo = etd.plusDays(thresholdLimit + 1).plusSeconds(-1);

                var etaAndETDCriteria = CommonUtils.andCriteria("eta", thresholdETAFrom, ">=", consolListRequest);
                etaAndETDCriteria = CommonUtils.andCriteria("eta", thresholdETATo, "<=", etaAndETDCriteria);
                etaAndETDCriteria = CommonUtils.andCriteria("etd", thresholdETDFrom, ">=", etaAndETDCriteria);
                etaAndETDCriteria = CommonUtils.andCriteria("etd", thresholdETDTo, "<=", etaAndETDCriteria);

                var priorityList = masterLists.stream()
                        .filter(x -> x.getItemType() == (int)MasterDataType.CONSOLIDATION_CHECK_ORDER.getId())
                        .sorted((y1, y2) -> {
                            int itd1 = Integer.parseInt(y1.getItemDescription());
                            int itd2 = Integer.parseInt(y2.getItemDescription());
                            return Integer.compare(itd1, itd2);
                        })
                        .map(EntityTransferMasterLists::getItemValue)
                        .toList();
                for (var item : priorityList){
                    switch (item.toUpperCase()){
                        case "VOYAGE NUMBER":
                            if(StringUtility.isNotEmpty(request.getVoyageNumber())){
                                consolListRequest = CommonUtils.andCriteria("voyage", request.getVoyageNumber(), "=", etaAndETDCriteria);
                                isConditionSatisfied = true;
                                response.setFilteredDetailName("Voyage Number");
                            }
                            break;
                        case "VESSEL NAME":
                            if(StringUtility.isNotEmpty(request.getVessel())){
                                consolListRequest = CommonUtils.andCriteria("vessel", request.getVessel(), "=", etaAndETDCriteria);
                                isConditionSatisfied = true;
                                response.setFilteredDetailName("Vessel Name");
                            }
                            break;
                        case "ORIGIN PORT/ DESTINATION PORT":
                            if(StringUtility.isNotEmpty(request.getPol()) && StringUtility.isNotEmpty(request.getPod())){
                                consolListRequest = CommonUtils.andCriteria("originPort", request.getPol(), "=", etaAndETDCriteria);
                                consolListRequest = CommonUtils.andCriteria("destinationPort", request.getPod(), "=", consolListRequest);
                                isConditionSatisfied = true;
                                response.setFilteredDetailName("Origin Port/ Destination Port");
                            }
                            break;
                        default:
                    }
                    if (isConditionSatisfied)
                        break;
                }
            }
            if (isConditionSatisfied){
                Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(consolListRequest, ConsolidationDetails.class, tableNames);
                Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
                List<ConsolidationDetailsResponse> consolidationDetailsResponseList = jsonHelper.convertValueToList(consolidationDetailsPage.getContent(), ConsolidationDetailsResponse.class);

                for (var console : consolidationDetailsResponseList){
                    if(console.getConsolidationNumber() == null)
                        console.setConsolidationNumber("");
                    if(console.getCarrierDetails().getVoyage() == null)
                        console.getCarrierDetails().setVoyage("");
                }
                response.setConsolidationDetailsList(consolidationDetailsResponseList);
                return ResponseHelper.buildSuccessResponse(response, consolidationDetailsPage.getTotalPages(),
                        consolidationDetailsPage.getTotalElements());
            }
        }

        return ResponseHelper.buildSuccessResponse(response);
    }

    @Override
    public ResponseEntity<IRunnerResponse> getAutoUpdateGoodsAndHandlingInfo(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            Long id = commonRequestModel.getId();
            Optional<ConsolidationDetails> consolidationDetailsOptional = consolidationDetailsDao.findById(id);
            if(!consolidationDetailsOptional.isPresent()) {
                log.debug(ConsolidationConstants.CONSOLIDATION_DETAILS_NULL_FOR_GIVEN_ID_ERROR, id);
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ConsolidationDetails consolidationDetails = consolidationDetailsOptional.get();
            ConsolidationDetailsResponse consolidationDetailsResponse = jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class);
            calculateDescOfGoodsAndHandlingInfo(consolidationDetails, consolidationDetailsResponse, true);
            return ResponseHelper.buildSuccessResponse(consolidationDetailsResponse.getContainersList());
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getContainerPackSummary(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            Long id = commonRequestModel.getId();
            ContainerPackSummaryDto response = new ContainerPackSummaryDto();
            response.setPacksList(new ArrayList<>());
            List<ConsoleShipmentMapping> consoleShipmentMappingList = consoleShipmentMappingDao.findByConsolidationId(id);
            if(consoleShipmentMappingList != null && consoleShipmentMappingList.size() > 0) {
                ListCommonRequest listCommonRequest = andCriteria(Constants.SHIPMENT_ID, consoleShipmentMappingList.stream().map(ConsoleShipmentMapping::getShipmentId).toList(), "IN", null);
                andCriteria(Constants.CONTAINER_ID, "", "ISNOTNULL", listCommonRequest);
                Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
                Map<Long, ShipmentDetails> shipMap = new HashMap<>();
                Map<Long, Containers> contMap = new HashMap<>();
                if(packings != null && packings.getContent() != null && !packings.getContent().isEmpty()) {
                    for (Packing packing : packings.getContent()) {
                        ContainerPackSummaryDto.PacksList packsList = jsonHelper.convertValue(packing, ContainerPackSummaryDto.PacksList.class);
                        ShipmentDetails shipmentDetails = getShipmentFromMap(shipMap, packing.getShipmentId());
                        packsList.setShipmentClient(jsonHelper.convertValue(shipmentDetails.getClient(), PartiesResponse.class));
                        packsList.setShipmentMasterBill(shipmentDetails.getMasterBill());
                        packsList.setShipmentHouseBill(shipmentDetails.getHouseBill());
                        packsList.setShipmentNumber(shipmentDetails.getShipmentId());
                        try { packsList.setShipmentStatus(ShipmentStatus.fromValue(shipmentDetails.getStatus())); }
                        catch (Exception e) { }
                        try { Containers containers = getContainerFromMap(contMap, packing.getContainerId());
                            packsList.setContainerNumber(containers.getContainerNumber()); }
                        catch (Exception e) { }
                        response.getPacksList().add(packsList);
                    }
                }
            }
            try {
                Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
                List<MasterListRequest> listRequests = new ArrayList<>();
                if(!Objects.isNull(response.getPacksList()))
                    response.getPacksList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, ContainerPackSummaryDto.PacksList.class, fieldNameKeyMap, ContainerPackSummaryDto.PacksList.class.getSimpleName() )));
                MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
                masterListRequestV2.setMasterListRequests(listRequests);
                masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));
                Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
                masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST);
                if(!Objects.isNull(response.getPacksList()))
                    response.getPacksList().forEach(r -> r.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ContainerPackSummaryDto.PacksList.class.getSimpleName()), CacheConstants.MASTER_LIST)));
            } catch (Exception e) { }
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getDefaultConsolidation() {
        String responseMsg;
        try {
            var tenantSettings = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
            // Populate shipment details on basis of tenant settings
            ConsolidationDetailsResponse response = new ConsolidationDetailsResponse();
            response.setCarrierDetails(new CarrierDetailResponse());
            response.setTransportMode(tenantSettings.getDefaultTransportMode());
            response.setContainerCategory(tenantSettings.getDefaultContainerType());
            response.setShipmentType(tenantSettings.getDefaultShipmentType());
            response.setBol((response.getTransportMode() == null || !response.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) ? generateCustomBolNumber() : null);
            if(!Objects.equals(null, response.getTransportMode()) && response.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_SEA)) {
                response.setModeOfBooking(Constants.INTTRA);
            }
            response.setCreatedBy(UserContext.getUser().getUsername());
            response.setCreatedAt(LocalDateTime.now());
            response.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));
            try {
                log.info("Fetching Tenant Model");
                TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
                List<UnlocationsResponse> unlocationsResponse = masterDataUtils.fetchUnlocationByOneIdentifier(EntityTransferConstants.ID, StringUtility.convertToString(tenantModel.getUnloco()));
                if (!Objects.isNull(unlocationsResponse) && !unlocationsResponse.isEmpty())
                    response.setPlaceOfIssue(unlocationsResponse.get(0).getLocationsReferenceGUID());
            } catch (Exception e){
                log.error("Failed in fetching tenant data from V1 with error : {}", e);
            }
            this.createConsolidationPayload(null, response);

            return ResponseHelper.buildSuccessResponse(response);
        } catch(Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getIdFromGuid(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getGuid() == null) {
                log.error("Request Guid is null for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findByGuid(UUID.fromString(request.getGuid()));
            if (!consolidationDetails.isPresent()) {
                log.debug("Consolidation Details is null for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info(ConsolidationConstants.CONSOLIDATION_DETAILS_FETCHED_SUCCESSFULLY, request.getGuid(), LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(ConsolidationDetailsResponse.builder().id(consolidationDetails.get().getId()).build());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    @Override
    public ResponseEntity<IRunnerResponse> getGuidFromId(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(request.getId());
            if (!consolidationDetails.isPresent()) {
                log.debug("Consolidation Details is null for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info(ConsolidationConstants.CONSOLIDATION_DETAILS_FETCHED_SUCCESSFULLY, request.getGuid(), LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(ConsolidationDetailsResponse.builder().guid(consolidationDetails.get().getGuid()).build());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    @Override
    public ResponseEntity<IRunnerResponse> generateCustomHouseBLNumber() throws RunnerException {
        try {
            return ResponseHelper.buildSuccessResponse(GenerateCustomHblResponse.builder().hblNumber(generateCustomBolNumber()).build());
        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    public String generateCustomBolNumber() {
        String res = null;
        ShipmentSettingsDetails tenantSetting = ShipmentSettingsDetailsContext.getCurrentTenantSettings();

        if(tenantSetting.getConsolidationLite() != null && tenantSetting.getConsolidationLite() && tenantSetting.getBolNumberGeneration() == null) {
            return  res;
        }

        if(tenantSetting.getBolNumberGeneration() != null) {
            res = tenantSetting.getBolNumberPrefix() != null ? tenantSetting.getBolNumberPrefix() : "";
            switch(tenantSetting.getBolNumberGeneration()) {
                case Random :
                    res += StringUtility.getRandomString(10);
                    break;
                default :
                    String serialNumber = v1Service.getMaxConsolidationId();
                    res += serialNumber;
                    break;
            }
        }

        return res;
    }

    // Create Auto event

    public void autoGenerateEvents(ConsolidationDetails consolidationDetails) {
        Events response = null;
        response = createAutomatedEvents(consolidationDetails, Constants.CONCRTD);

        if(response != null) {
            if (consolidationDetails.getEventsList() == null)
                consolidationDetails.setEventsList(new ArrayList<>());
            consolidationDetails.getEventsList().add(response);
        }
    }

    private Events createAutomatedEvents(ConsolidationDetails consolidationDetails, String eventCode) {
        Events events = new Events();
        // Set event fields from consolidation
        events.setActual(LocalDateTime.now());
        events.setEstimated(LocalDateTime.now());
        events.setSource(Constants.CARGO_RUNNER);
        events.setIsPublicTrackingEvent(true);
        events.setEntityType(Constants.CONSOLIDATION);
        events.setEntityId(consolidationDetails.getId());
        events.setTenantId(TenantContext.getCurrentTenant());
        events.setEventCode(eventCode);
        // Persist the event
        eventDao.save(events);
        return events;
    }
    public ResponseEntity<IRunnerResponse> validateMawbNumber(CommonRequestModel commonRequestModel) {
        ValidateMawbNumberRequest request = (ValidateMawbNumberRequest) commonRequestModel.getData();
        ValidateMawbNumberResponse response = ValidateMawbNumberResponse.builder().success(true).build();
        if(Strings.isNullOrEmpty(request.getMawb())){
            return ResponseHelper.buildSuccessResponse(response);
        }
        if(!consolidationDetailsDao.isMAWBNumberValid(request.getMawb())){
            throw new ValidationException("Please enter a valid MAWB number.");
        }
        String mawbAirlineCode = request.getMawb().substring(0, 3);
        V1DataResponse v1DataResponse =  fetchCarrierDetailsFromV1(mawbAirlineCode, request.getType());
        List<CarrierResponse> carrierDetails = jsonHelper.convertValueToList(v1DataResponse.entities, CarrierResponse.class);
        if (carrierDetails == null || carrierDetails.isEmpty())
            throw new ValidationException("Airline for the entered MAWB Number doesn't exist in Carrier Master");
        var correspondingCarrier = carrierDetails.get(0).getItemValue();
        if (!Strings.isNullOrEmpty(request.getShippingLine()) && !request.getShippingLine().equals(correspondingCarrier)){
            response = ValidateMawbNumberResponse.builder()
                    .success(false)
                    .message("MAWB Number prefix is not matching with entered Flight Carrier. Do you want to proceed?")
                    .build();
        }
        return ResponseHelper.buildSuccessResponse(response);
    }
    private V1DataResponse fetchCarrierDetailsFromV1(String mawbAirlineCode, String type) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>();
        criteria.add(Arrays.asList(List.of("AirlineCode"), "=", mawbAirlineCode));
        criteria.add("and");
        criteria.add(Arrays.asList(List.of("HasAirPort"), "=", 1));
        request.setCriteriaRequests(criteria);
        CarrierListObject carrierListObject = new CarrierListObject();
        carrierListObject.setListObject(request);
        carrierListObject.setType(type);
        V1DataResponse response = v1Service.fetchCarrierMasterData(carrierListObject, true);
        return response;
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> updateConsoleBookingFields(CommonRequestModel commonRequestModel){
        ConsoleBookingRequest request = (ConsoleBookingRequest) commonRequestModel.getData();
        int rowsAffected = consolidationDetailsDao.updateConsoleBookingFields(request);
        if(rowsAffected == 0){
            throw new ValidationException("No Consolidation Exist with given guid: " + request.getGuid());
        }
        return ResponseHelper.buildSuccessResponse();
    }

    public ResponseEntity<IRunnerResponse> showCreateBooking(String operation) throws RunnerException {
        if(operation.equals("CREATE") && (PermissionsContext.getPermissions(Constants.CARRIER_BOOKING_CREATE) == null || PermissionsContext.getPermissions(Constants.CARRIER_BOOKING_CREATE).size() == 0)){
            throw new RunnerException("You don't have necessary permission to create Carrier Booking.");
        }

        if(operation.equals("VIEW") && (PermissionsContext.getPermissions(Constants.CARRIER_BOOKING_VIEW) == null || PermissionsContext.getPermissions(Constants.CARRIER_BOOKING_VIEW).size() == 0)){
            throw new RunnerException("You don't have necessary permission to view Carrier Booking.");
        }

        return new ResponseEntity<>(null, HttpStatus.OK);
    }
}
