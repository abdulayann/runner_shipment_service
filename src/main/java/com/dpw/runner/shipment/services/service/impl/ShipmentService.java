package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.Kafka.Dto.KafkaResponse;
import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
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
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerPartialListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.config.LocalTimeZoneHelper;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.TrackingService.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.patchRequest.CarrierPatchRequest;
import com.dpw.runner.shipment.services.dto.patchRequest.ShipmentPatchRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.request.*;
import com.dpw.runner.shipment.services.dto.v1.response.*;
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
import com.dpw.runner.shipment.services.mapper.ShipmentDetailsMapper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.service_bus.AzureServiceBusTopic;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.syncing.AuditLogsSyncRequest;
import com.dpw.runner.shipment.services.syncing.Entity.AuditLogRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PartyRequestV2;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.impl.SyncEntityConversionService;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IHblSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.github.bohnman.squiggly.Squiggly;
import com.github.bohnman.squiggly.util.SquigglyUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.modelmapper.ModelMapper;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.util.StringUtils;

import javax.servlet.http.HttpServletResponse;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.StringUtility.isNotEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ShipmentService implements IShipmentService {

    ExecutorService executorService = Executors.newFixedThreadPool(10);
    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private ShipmentDetailsMapper shipmentDetailsMapper;
    @Autowired
    private CarrierDetailsMapper carrierDetailsMapper;

    @Autowired
    private CSVParsingUtil<ShipmentDetails> parser;

    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private IAdditionalDetailDao additionalDetailDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IPackingsSync packingsSync;

    @Autowired
    private IPackingService packingService;

    @Autowired
    private IContainerService containerService;

    @Autowired
    private ITruckDriverDetailsDao truckDriverDetailsDao;
    @Autowired
    private IBookingCarriageDao bookingCarriageDao;

    @Autowired
    private IELDetailsDao elDetailsDao;

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private INotesDao notesDao;

    @Autowired
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    @Autowired
    private IReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private IRoutingsDao routingsDao;

    @Autowired
    private IServiceDetailsDao serviceDetailsDao;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private TransactionTemplate transactionTemplate;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    UserContext userContext;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private SyncEntityConversionService syncEntityConversionService;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private IConsolidationService consolidationService;

    @Autowired
    private IEventService eventService;

    @Autowired
    private ISBUtils sbUtils;

    @Autowired
    private ISBProperties isbProperties;

    @Autowired
    private AzureServiceBusTopic azureServiceBusTopic;
    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private IAwbDao awbDao;

    @Autowired
    private MasterDataKeyUtils masterDataKeyUtils;

    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    IShipmentSync shipmentSync;
    @Autowired
    IHblService hblService;
    @Autowired
    IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private IConsolidationSync consolidationSync;

    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private ISequenceIncrementorDao sequenceIncrementorDao;

    @Autowired
    private IOrderManagementAdapter orderManagementAdapter;

    @Value("${shipmentsKafka.queue}")
    private String senderQueue;

    @Autowired
    private KafkaProducer producer;

    @Autowired
    private ITrackingServiceAdapter trackingServiceAdapter;

    @Autowired
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Autowired
    private GetNextNumberHelper getNextNumberHelper;

    @Autowired
    private IHblSync hblSync;

    @Autowired
    private V1ServiceUtil v1ServiceUtil;

    @Autowired
    private ProductIdentifierUtility productEngine;
    private SecureRandom rnd = new SecureRandom();

    @Autowired @Lazy
    private BookingIntegrationsUtility bookingIntegrationsUtility;
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
            Map.entry("clientOrgCode", RunnerEntityMapping.builder().tableName(Constants.CLIENT).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry("consignerOrgCode", RunnerEntityMapping.builder().tableName(Constants.CONSIGNER).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry("consigneeOrgCode", RunnerEntityMapping.builder().tableName(Constants.CONSIGNEE).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry("clientAddressCode", RunnerEntityMapping.builder().tableName(Constants.CLIENT).dataType(Integer.class).fieldName(Constants.ADDRESS_CODE).isContainsText(true).build()),
            Map.entry("consignerAddressCode", RunnerEntityMapping.builder().tableName(Constants.CONSIGNER).dataType(String.class).fieldName(Constants.ADDRESS_CODE).isContainsText(true).build()),
            Map.entry("consigneeAddressCode", RunnerEntityMapping.builder().tableName(Constants.CONSIGNEE).dataType(String.class).fieldName(Constants.ADDRESS_CODE).isContainsText(true).build()),
            Map.entry("houseBill", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("houseBill").isContainsText(true).build()),
            Map.entry("houseBillType", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("houseBillType").isContainsText(true).build()),
            Map.entry(Constants.TRANSPORT_MODE, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.TRANSPORT_MODE).isContainsText(true).build()),
            Map.entry("releaseType", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("releaseType").isContainsText(true).build()),
            Map.entry("deliveryMode", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("deliveryMode").isContainsText(true).build()),
            Map.entry(Constants.DIRECTION, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.DIRECTION).isContainsText(true).build()),
            Map.entry("shipmentType", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("shipmentType").isContainsText(true).build()),
            Map.entry(Constants.STATUS, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName(Constants.STATUS).build()),
            Map.entry("guid", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(UUID.class).fieldName("guid").build()),
            Map.entry("source", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("source").isContainsText(true).build()),
            Map.entry(Constants.JOB_TYPE, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.JOB_TYPE).isContainsText(true).build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("createdBy").isContainsText(true).build()),
            Map.entry("serviceType", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("serviceType").isContainsText(true).build()),
            Map.entry("masterBill", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("masterBill").isContainsText(true).build()),
            Map.entry("bookingReference", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("bookingReference").isContainsText(true).build()),
            Map.entry("consolRef", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("consolRef").isContainsText(true).build()),
            Map.entry("salesAgent", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Long.class).fieldName("salesAgent").build()),
            Map.entry("paymentTerms", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("paymentTerms").isContainsText(true).build()),
            Map.entry("incoterms", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("incoterms").isContainsText(true).build()),
            Map.entry(Constants.SHIPMENT_ID, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.SHIPMENT_ID).isContainsText(true).build()),
            Map.entry("isDomestic", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Boolean.class).fieldName("isDomestic").build()),
            Map.entry("assignedTo", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName("assignedTo").build()),
            Map.entry("additionalTerms", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("additionalTerms").isContainsText(true).build()),
            Map.entry("goodsDescription", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("goodsDescription").isContainsText(true).build()),
            Map.entry("createdAt", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(LocalDateTime.class).fieldName("createdAt").build()),
            Map.entry("updatedAt", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(LocalDateTime.class).fieldName("updatedAt").build()),
            Map.entry("deliveryEstimated", RunnerEntityMapping.builder().tableName(Constants.DELIVERY_DETAILS).dataType(LocalDateTime.class).fieldName("estimatedPickupOrDelivery").build()),
            Map.entry("deliveryActual", RunnerEntityMapping.builder().tableName(Constants.DELIVERY_DETAILS).dataType(LocalDateTime.class).fieldName("actualPickupOrDelivery").build()),
            Map.entry("deliveryRequiredBy", RunnerEntityMapping.builder().tableName(Constants.DELIVERY_DETAILS).dataType(LocalDateTime.class).fieldName("requiredBy").build()),
            Map.entry("pickupEstimated", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(LocalDateTime.class).fieldName("estimatedPickupOrDelivery").build()),
            Map.entry("pickupActual", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(LocalDateTime.class).fieldName("actualPickupOrDelivery").build()),
            Map.entry("pickupRequiredBy", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(LocalDateTime.class).fieldName("requiredBy").build()),
            Map.entry("screeningStatus", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("screeningStatus").build()),
            Map.entry("paidPlace", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(Long.class).fieldName("paidPlace").build()),
            Map.entry("placeOfIssue", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(Long.class).fieldName("placeOfIssue").build()),
            Map.entry("dateOfIssue", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(LocalDateTime.class).fieldName("dateOfIssue").build()),
            Map.entry("dateOfReceipt", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(LocalDateTime.class).fieldName("dateOfReceipt").build()),
            Map.entry("goodsCo", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("goodsCo").build()),
            Map.entry("BOEDate", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(LocalDateTime.class).fieldName("BOEDate").build()),
            Map.entry("boeNumber", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("BOENumber").isContainsText(true).build()),
            Map.entry(Constants.SHIPPING_LINE, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.SHIPPING_LINE).isContainsText(true).build()),
            Map.entry(Constants.VESSEL, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.VESSEL).build()),
            Map.entry(Constants.VOYAGE, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.VOYAGE).build()),
            Map.entry("origin", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("origin").build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destination").build()),
            Map.entry(Constants.ORIGIN_PORT, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.ORIGIN_PORT).build()),
            Map.entry(Constants.DESTINATION_PORT, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.DESTINATION_PORT).build()),
            Map.entry("eta", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("eta").build()),
            Map.entry("etd", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("etd").build()),
            Map.entry("ata", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("ata").build()),
            Map.entry("atd", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(LocalDateTime.class).fieldName("atd").build()),
            Map.entry("weight", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("weight").build()),
            Map.entry("weightUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("weightUnit").build()),
            Map.entry("volume", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("volume").build()),
            Map.entry("volumeUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("volumeUnit").build()),
            Map.entry("volumetricWeight", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("volumetricWeight").build()),
            Map.entry("volumetricWeightUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("volumetricWeightUnit").build()),
            Map.entry("chargable", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("chargable").build()),
            Map.entry("chargeableUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("chargeableUnit").build()),
            Map.entry("netWeight", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(BigDecimal.class).fieldName("netWeight").build()),
            Map.entry("netWeightUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("netWeightUnit").build()),
            Map.entry("noOfPacks", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName("noOfPacks").build()),
            Map.entry("packsUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("packsUnit").build()),
            Map.entry("innerPacks", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Integer.class).fieldName("innerPacks").build()),
            Map.entry("innerPackUnit", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("innerPackUnit").build()),
            Map.entry("jobStatus", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("jobStatus").build()),
            Map.entry("containerNumber", RunnerEntityMapping.builder().tableName(Constants.CONTAINERS_LIST).dataType(String.class).fieldName("containerNumber").build()),
            Map.entry("containerCode", RunnerEntityMapping.builder().tableName(Constants.CONTAINERS_LIST).dataType(String.class).fieldName("containerCode").build()),
            Map.entry("id", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Long.class).fieldName("id").build()),
            Map.entry("consolidationNumber", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_LIST).dataType(String.class).fieldName("consolidationNumber").build()),
            Map.entry("orderNumber", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.ORDER_MANAGEMENT_ID).build()),
            Map.entry("referenceNumber", RunnerEntityMapping.builder().tableName("referenceNumbersList").dataType(String.class).fieldName("referenceNumber").build()),
            Map.entry("activityType", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("activityType").build()),
            Map.entry("goodsCO", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(String.class).fieldName("goodsCO").build()),
            Map.entry("route", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName("route").build()),
            Map.entry("cargoFinanceBooking", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(Boolean.class).fieldName("cargoFinanceBooking").build()),
            Map.entry("isCmsHBLSent", RunnerEntityMapping.builder().tableName(Constants.ADDITIONAL_DETAILS).dataType(Boolean.class).fieldName("isCmsHBLSent").build()),
            Map.entry(Constants.ORDER_MANAGEMENT_ID, RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).dataType(String.class).fieldName(Constants.ORDER_MANAGEMENT_ID).isContainsText(true).build()),
            Map.entry(Constants.FLIGHT_NUMBER, RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName(Constants.FLIGHT_NUMBER).build()),
            Map.entry("consolidationId", RunnerEntityMapping.builder().tableName(Constants.CONSOLIDATION_LIST).dataType(Long.class).fieldName("id").build()),
            Map.entry("voyageOrFlightNumber", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("voyageOrFlightNumber").build()),
            Map.entry("shipperRef", RunnerEntityMapping.builder().tableName(Constants.PICKUP_DETAILS).dataType(String.class).fieldName("shipperRef").build())
    );

    @Override
    @Transactional
    public List<ShipmentDetails> createTestShipment(Integer count) throws RunnerException {
        List<ShipmentDetails> response = new ArrayList<>();
        /**
         * BL details
         * Measurements
         * carrier
         * pickup
         * Delivery* *
         * Shipment details
         * Parties*
         * * * * * * *
         * * * */

        for (int i = 0; i < count; i++) {

            ShipmentDetails shipmentDetail = createShipmentData();
            /**
             * Carrier Details*
             */

            shipmentDetail = shipmentDao.save(shipmentDetail, false);
            pushShipmentDataToDependentService(shipmentDetail, true);
        }

        return response;
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchShipments(CommonRequestModel commonRequestModel) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS, Constants.PICKUP_DETAILS, Constants.DELIVERY_DETAILS));
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
        return ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements());
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(shipmentDetail -> {
            ShipmentListResponse response = modelMapper.map(shipmentDetail, ShipmentListResponse.class);
            containerCountUpdate(shipmentDetail, response);
            setEventData(shipmentDetail, response);
            if (shipmentDetail.getStatus() != null && shipmentDetail.getStatus() < ShipmentStatus.values().length)
                response.setShipmentStatus(ShipmentStatus.values()[shipmentDetail.getStatus()].toString());
            responseList.add(response);
        });
        try {
            var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID)), executorService);
            var containerDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setContainerTeuData(lst, responseList)), executorService);
            var billDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchBillDataForShipments(lst, responseList)), executorService);
            var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchVesselForList(responseList)), executorService);
            CompletableFuture.allOf(locationDataFuture, containerDataFuture, billDataFuture, vesselDataFuture).join();
        }
        catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
        }
        return responseList;
    }

    private void containerCountUpdate(ShipmentDetails shipmentDetail, ShipmentListResponse response) {
        Long container20Count = 0L;
        Long container40Count = 0L;
        Long container20GPCount = 0L;
        Long container20RECount = 0L;
        Long container40GPCount = 0L;
        Long container40RECount = 0L;
        Set<String> containerNumber = new HashSet<>();
        if (shipmentDetail.getContainersList() != null) {
            for (Containers container : shipmentDetail.getContainersList()) {
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
                if (StringUtility.isNotEmpty(container.getContainerNumber())) {
                    containerNumber.add(container.getContainerNumber());
                }
            }
//            container20Count = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().contains(Constants.Cont20)).count();
//            container40Count = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().contains(Constants.Cont40)).count();
//            container20GPCount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont20GP)).count();
//            container20RECount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont20RE)).count();
//            container40GPCount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont40GP)).count();
//            container40RECount = shipmentDetail.getContainersList().stream().filter(container -> container.getContainerCode() != null && container.getContainerCode().equals(Constants.Cont40RE)).count();
        }
        response.setContainer20Count(container20Count);
        response.setContainer40Count(container40Count);
        response.setContainer20GPCount(container20GPCount);
        response.setContainer20RECount(container20RECount);
        response.setContainer40GPCount(container40GPCount);
        response.setContainer40RECount(container40RECount);
        response.setContainerNumbers(containerNumber);
    }

    private void setEventData(ShipmentDetails shipmentDetail, ShipmentListResponse response) {
        if (shipmentDetail.getEventsList() != null) {
            for (Events events : shipmentDetail.getEventsList()) {
                if (StringUtility.isNotEmpty(events.getEventCode())) {
                    if (events.getEventCode().equalsIgnoreCase(Constants.INVGNTD)) {
                        response.setInvoiceDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(Constants.TAXSG)) {
                        response.setTaxDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(Constants.CSEDI)) {
                        response.setCustomsFilingDate(events.getActual());
                    } else if (events.getEventCode().equalsIgnoreCase(Constants.AMSEDI)) {
                        response.setAmsFilingDate(events.getActual());
                    }
                }
            }
        }
    }

    private List<Parties> createParties(ShipmentDetails shipmentDetails) {
        List<Parties> parties = new ArrayList<>();
        for (String partyType : PARTY_TYPE) {
            Parties party = Parties.builder()
                    .type(partyType).orgCode(generateString(7)).addressCode(generateString(7))
                    .orgData(ORG).addressData(ADDRESS)
                    .entityId(shipmentDetails.getId()).entityType("SHIPMENT")
                    .build();
            party.setTenantId(1);
            parties.add(party);
        }
        parties = partiesDao.saveAll(parties);
        return parties;
    }


    private ShipmentDetails createShipmentData() {
        int random = rnd.nextInt(100);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().direction(DIRECTIONS.get(random % DIRECTIONS.size())).status(1)
                .source(SOURCE.get(random % SOURCE.size())).transportMode(TRANSPORT_MODES.get(random % TRANSPORT_MODES.size())).shipmentType(SHIPMENT_TYPE.get(random % SHIPMENT_TYPE.size()))
                .houseBill(generateString(10)).masterBill(generateString(10)).bookingReference(generateString(10)).consolRef(generateString(10)).paymentTerms(generateString(3))
                .goodsDescription(generateString(10)).additionalTerms(generateString(10))
                .build();
        shipmentDetails.setTenantId(1);
        return shipmentDetails;
    }

    private String generateString(int length) {
        StringBuilder salt = new StringBuilder();
        while (salt.length() < length) {
            salt.append(Constants.SALT_CHARS.charAt(Math.abs(this.rnd.nextInt() * Constants.SALT_CHARS.length()) % Constants.SALT_CHARS.length()));
        }
        return salt.toString();
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> createFromBooking(CommonRequestModel commonRequestModel)
    {
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Shipment Create From Booking with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        ShipmentDetails shipmentDetails = jsonHelper.convertValue(request, ShipmentDetails.class);
        try {
            if(request.getConsolidationList() != null)
                shipmentDetails.setConsolidationList(jsonHelper.convertValueToList(request.getConsolidationList(), ConsolidationDetails.class));
            if(request.getContainersList() != null)
                shipmentDetails.setContainersList(jsonHelper.convertValueToList(request.getContainersList(), Containers.class));
            shipmentDetails = getShipment(shipmentDetails);
            ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
            if(shipmentSettingsDetails.getAutoEventCreate() != null && shipmentSettingsDetails.getAutoEventCreate())
                autoGenerateCreateEvent(shipmentDetails);
            autoGenerateEvents(shipmentDetails, null);
            Long shipmentId = shipmentDetails.getId();
            List<Packing> updatedPackings = new ArrayList<>();
            if (request.getPackingList() != null) {
                updatedPackings = packingDao.saveEntityFromShipment(jsonHelper.convertValueToList(request.getPackingList(), Packing.class), shipmentId);
                shipmentDetails.setPackingList(updatedPackings);
            }
            List<RoutingsRequest> routingsRequest = request.getRoutingsList();
            if (routingsRequest != null)
                shipmentDetails.setRoutingsList(routingsDao.saveEntityFromShipment(jsonHelper.convertValueToList(routingsRequest, Routings.class), shipmentId));
            Hbl hbl = null;
            if(shipmentDetails.getContainersList() != null && shipmentDetails.getContainersList().size() > 0) {
                hbl = hblService.checkAllContainerAssigned(shipmentDetails, shipmentDetails.getContainersList(), updatedPackings);
            }

            List<NotesRequest> notesRequest = request.getNotesList();
            if (notesRequest != null) {
                for(NotesRequest req : notesRequest) {
                    req.setEntityId(shipmentId);
                }
            }
            if (notesRequest != null) {
                for(NotesRequest req : notesRequest) {
                    notesDao.save(jsonHelper.convertValue(req, Notes.class));
                }
            }
            String transactionId = shipmentDetails.getGuid().toString();
            pushShipmentDataToDependentService(shipmentDetails, true);
            try {
                shipmentDetails.setNotesList(null);
                shipmentSync.syncFromBooking(shipmentDetails, null, notesRequest);
            } catch (Exception e){
                log.error(SyncingConstants.ERROR_SYNCING_SHIPMENTS, e);
            }

            auditLogService.addAuditLog(
                AuditLogMetaData.builder()
                        .newData(shipmentDetails)
                        .prevData(null)
                        .parent(ShipmentDetails.class.getSimpleName())
                        .parentId(shipmentDetails.getId())
                        .operation(DBOperationType.CREATE.name()).build()
            );
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class));
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        //ExecutorService executorService = Executors.newFixedThreadPool(100);

        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Shipment Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        ShipmentDetails shipmentDetails = jsonHelper.convertCreateValue(request, ShipmentDetails.class);
        if(request.getConsolidationList() != null)
            shipmentDetails.setConsolidationList(jsonHelper.convertValueToList(request.getConsolidationList(), ConsolidationDetails.class));

        try {
            ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();

            boolean syncConsole = beforeSave(shipmentDetails, null, true, request, shipmentSettingsDetails);

            shipmentDetails = getShipment(shipmentDetails);
            Long shipmentId = shipmentDetails.getId();


            if(shipmentDetails.getContainersList() != null && !shipmentDetails.getContainersList().isEmpty())
            {
                for (Containers container: shipmentDetails.getContainersList()) {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                        .newData(container)
                                        .prevData(null)
                                        .parent(ShipmentDetails.class.getSimpleName())
                                        .parentId(shipmentId)
                                        .operation(DBOperationType.CREATE.name()).build()
                        );
                    } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException | InvocationTargetException | NoSuchMethodException e) {
                        log.error(e.getMessage());
                    }
                }
            }

            afterSave(shipmentDetails, null, true, request, shipmentSettingsDetails, syncConsole);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(shipmentDetails)
                            .prevData(null)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(shipmentDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

        } catch (Exception e) {
            log.error("Error occurred due to: " + e.getStackTrace());
            log.error(e.getMessage());
            throw new ValidationException(e.getMessage());
        }


//        CompletableFuture.allOf(createCallToAdditionalDetails, createCallToContainers, createCallToPackings, createCallToBookingCarriages, createCallToElDetails, createCallToEvents, createCallToFileRepos, createCallToJobs, createCallToNotes, createCallToReferenceNumbers, createCallToRoutings, createCallToServiceDetails, createCallToPickupDelivery, createCallToParties, createCallToCarrierDetails).join();
//        executorService.shutdownNow();
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class));
    }

    ShipmentDetails getShipment(ShipmentDetails shipmentDetails) throws RunnerException {
        if(shipmentDetails.getShipmentId() == null){
            shipmentDetails.setShipmentId(generateShipmentId(shipmentDetails));
        }
        shipmentDetails = shipmentDao.save(shipmentDetails, false);
        return shipmentDetails;
        //shipmentDetails = shipmentDao.findById(shipmentDetails.getId()).get();
    }


    private void createPartiesAsync(ShipmentDetails shipmentDetails, List<PartiesRequest> partiesRequest) {
        partiesRequest.forEach(parties -> {
            createParties(shipmentDetails, parties);
        });
    }

    private void createServiceDetailsAsync(ShipmentDetails shipmentDetails, List<ServiceDetailsRequest> serviceDetailsRequest) {
        serviceDetailsRequest.forEach(serviceDetails -> {
            createServiceDetail(shipmentDetails, serviceDetails);
        });
    }

    private void createRoutingsAsync(ShipmentDetails shipmentDetails, List<RoutingsRequest> routingsRequest) {
        routingsRequest.forEach(routing -> {
            createRouting(shipmentDetails, routing);
        });
    }

    private void createReferenceNumbersAsync(ShipmentDetails shipmentDetails, List<ReferenceNumbersRequest> referenceNumbersRequest) {
        referenceNumbersRequest.forEach(referenceNumber -> {
            createReferenceNumber(shipmentDetails, referenceNumber);
        });
    }

    private void createNotesAsync(ShipmentDetails shipmentDetails, List<NotesRequest> notesRequest) {
        notesRequest.forEach(notes -> {
            createNote(shipmentDetails, notes);
        });
    }

    private void createEventsAsync(ShipmentDetails shipmentDetails, List<EventsRequest> eventsRequest) {
        eventsRequest.forEach(event -> {
            createEvent(shipmentDetails, event);
        });
    }

    private void createElDetailsAsync(ShipmentDetails shipmentDetails, List<ELDetailsRequest> elDetailsRequest) {
        elDetailsRequest.forEach(elDetails -> {
            createElDetail(shipmentDetails, elDetails);
        });
    }

    private void createBookingCarriagesAsync(ShipmentDetails shipmentDetails, List<BookingCarriageRequest> bookingCarriageRequest) {
        bookingCarriageRequest.forEach(booking -> {
            createbookingCarriage(shipmentDetails, booking);
        });
    }

    private void createPackingsAsync(ShipmentDetails shipmentDetails, List<PackingRequest> packingRequest) {
        packingRequest.forEach(packing -> {
            createPacking(shipmentDetails, packing);
        });

    }

    @Transactional
    public void createbookingCarriage(ShipmentDetails shipmentDetails, BookingCarriageRequest bookingCarriageRequest) {
        bookingCarriageRequest.setShipmentId(shipmentDetails.getId());
        bookingCarriageDao.save(objectMapper.convertValue(bookingCarriageRequest, BookingCarriage.class));
    }

    @Transactional
    public void createElDetail(ShipmentDetails shipmentDetails, ELDetailsRequest elDetailsRequest) {
        elDetailsRequest.setShipmentId(shipmentDetails.getId());
        elDetailsDao.save(objectMapper.convertValue(elDetailsRequest, ELDetails.class));
    }

    @Transactional
    public void createEvent(ShipmentDetails shipmentDetails, EventsRequest eventsRequest) {
        eventsRequest.setEntityId(shipmentDetails.getId());
        eventsRequest.setEntityType(Constants.SHIPMENT);
        eventDao.save(objectMapper.convertValue(eventsRequest, Events.class));
    }

    @Transactional
    public void createNote(ShipmentDetails shipmentDetails, NotesRequest notesRequest) {
        notesRequest.setEntityId(shipmentDetails.getId());
        notesRequest.setEntityType(Constants.SHIPMENT);
        notesDao.save(objectMapper.convertValue(notesRequest, Notes.class));
    }

    @Transactional
    public void createParties(ShipmentDetails shipmentDetails, PartiesRequest partiesRequest) {
        partiesRequest.setEntityId(shipmentDetails.getId());
        partiesRequest.setEntityType("SHIPMENT");
        packingDao.save(objectMapper.convertValue(partiesRequest, Packing.class));
    }

    @Transactional
    public void createReferenceNumber(ShipmentDetails shipmentDetails, ReferenceNumbersRequest referenceNumbersRequest) {
        referenceNumbersRequest.setShipmentId(shipmentDetails.getId());
        referenceNumbersDao.save(objectMapper.convertValue(referenceNumbersRequest, ReferenceNumbers.class));
    }

    @Transactional
    public void createPacking(ShipmentDetails shipmentDetails, PackingRequest packingRequest) {
        packingRequest.setShipmentId(shipmentDetails.getId());
        packingDao.save(objectMapper.convertValue(packingRequest, Packing.class));
    }

    @Transactional
    public void createRouting(ShipmentDetails shipmentDetails, RoutingsRequest routingsRequest) {
        routingsRequest.setShipmentId(shipmentDetails.getId());
        routingsDao.save(objectMapper.convertValue(routingsRequest, Routings.class));
    }

    @Transactional
    public void createServiceDetail(ShipmentDetails shipmentDetails, ServiceDetailsRequest serviceDetailsRequest) {
        serviceDetailsRequest.setShipmentId(shipmentDetails.getId());
        serviceDetailsDao.save(objectMapper.convertValue(serviceDetailsRequest, ServiceDetails.class));
    }

    @Transactional
    public void createAdditionalDetail(ShipmentDetails shipmentDetails, AdditionalDetails additionalDetails) {
        additionalDetailDao.save(additionalDetails);
    }


    public Optional<ShipmentDetails> retrieveByIdOrGuid(ShipmentRequest request) throws RunnerException {
        String responseMsg;

        if (request == null) {
            log.error("Request is empty for Shipment update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        Optional<ShipmentDetails> oldEntity = Optional.ofNullable(null);

        if(request.getId()!=null){
            long id = request.getId();
            oldEntity=shipmentDao.findById(id);
            if (!oldEntity.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

        }

        else if(request.getGuid()!=null){
            UUID guid = request.getGuid();
            oldEntity= shipmentDao.findByGuid(guid);
            if (!oldEntity.isPresent()) {
                log.debug("Shipment Details is null for GUID {} with Request GUID {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
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
    public ResponseEntity<IRunnerResponse> createShipmentInV2(CustomerBookingRequest customerBookingRequest) throws RunnerException
    {
        List<ConsolidationDetailsRequest> consolidationDetails = new ArrayList<>();
        List<ContainerRequest> containerList = new ArrayList<>();
        List<Notes> notes = notesDao.findByEntityIdAndEntityType(customerBookingRequest.getId(), "CustomerBooking");
        if(customerBookingRequest.getCargoType().equals("FCL"))
        {
            ConsolidationDetailsRequest consolidationDetailsRequest = ConsolidationDetailsRequest.builder().
                    carrierDetails(CarrierDetailRequest.builder()
                            .origin(customerBookingRequest.getCarrierDetails().getOrigin())
                            .destination(customerBookingRequest.getCarrierDetails().getDestination())
                            .shippingLine(customerBookingRequest.getCarrierDetails().getShippingLine())
                            .vessel(customerBookingRequest.getCarrierDetails().getVessel())
                            .voyage(customerBookingRequest.getCarrierDetails().getVoyage())
                            .originPort(customerBookingRequest.getCarrierDetails().getOriginPort())
                            .destinationPort(customerBookingRequest.getCarrierDetails().getDestinationPort())
                            .flightNumber(customerBookingRequest.getCarrierDetails().getFlightNumber())
                            .build()
                    ).
                    consolidationType("STD").
                    transportMode(customerBookingRequest.getTransportType()).
                    containerCategory(customerBookingRequest.getCargoType()).
                    shipmentType(customerBookingRequest.getDirection()).
                    referenceNumber(customerBookingRequest.getBookingNumber()).
                    departureDetails(ArrivalDepartureDetailsRequest.builder().
                            firstForeignPort(customerBookingRequest.getCarrierDetails().getOrigin()).
                            lastForeignPort(customerBookingRequest.getCarrierDetails().getOrigin()).
                            type("Departure").
                            build()
                    ).
                    arrivalDetails(ArrivalDepartureDetailsRequest.builder().
                            firstForeignPort(customerBookingRequest.getCarrierDetails().getDestination()).
                            lastForeignPort(customerBookingRequest.getCarrierDetails().getDestination()).
                            type("Arrival").
                            build()
                    ).
                    containersList(customerBookingRequest.getContainersList()).
                    sourceTenantId(Long.valueOf(UserContext.getUser().TenantId)).
                    build();
            ResponseEntity<?> consolidationDetailsResponse = consolidationService.createFromBooking(CommonRequestModel.buildRequest(consolidationDetailsRequest));
            if(consolidationDetailsResponse != null)
            {
                ConsolidationDetailsResponse consolDetailsResponse = (ConsolidationDetailsResponse) (((RunnerResponse)consolidationDetailsResponse.getBody()).getData());
                ConsolidationDetailsRequest consolRequest = jsonHelper.convertValue(consolDetailsResponse, ConsolidationDetailsRequest.class);
                containerList = consolRequest.getContainersList();
                consolRequest.setContainersList(null);
                consolidationDetails.add(consolRequest);
            }
        }

        ShipmentRequest shipmentRequest = ShipmentRequest.builder().
                carrierDetails(CarrierDetailRequest.builder()
                        .origin(customerBookingRequest.getCarrierDetails().getOrigin())
                        .destination(customerBookingRequest.getCarrierDetails().getDestination())
                        .shippingLine(customerBookingRequest.getCarrierDetails().getShippingLine())
                        .vessel(customerBookingRequest.getCarrierDetails().getVessel())
                        .voyage(customerBookingRequest.getCarrierDetails().getVoyage())
                        .originPort(customerBookingRequest.getCarrierDetails().getOriginPort())
                        .destinationPort(customerBookingRequest.getCarrierDetails().getDestinationPort())
                        .flightNumber(customerBookingRequest.getCarrierDetails().getFlightNumber())
                        .build()
                ).
                contractId(customerBookingRequest.getContractId()).
                contractType(customerBookingRequest.getContractStatus()).
                noOfPacks(customerBookingRequest.getQuantity()).
                packsUnit(customerBookingRequest.getQuantityUnit()).
                weight(customerBookingRequest.getGrossWeight()).
                weightUnit(customerBookingRequest.getGrossWeightUnit()).
                volume(customerBookingRequest.getVolume()).
                volumeUnit(customerBookingRequest.getVolumeUnit()).
                volumetricWeight(customerBookingRequest.getWeightVolume()).
                volumetricWeightUnit(customerBookingRequest.getWeightVolumeUnit()).
                bookingReference(customerBookingRequest.getBookingNumber()).
                bookingCreatedDate(customerBookingRequest.getBookingDate()).
                shipmentCreatedOn(LocalDateTime.now()).
                client(createPartiesRequest(customerBookingRequest.getCustomer())).
                consignee(createPartiesRequest(customerBookingRequest.getConsignee())).
                consigner(createPartiesRequest(customerBookingRequest.getConsignor())).
                additionalDetails(AdditionalDetailRequest.builder().
                        notifyParty(createPartiesRequest(customerBookingRequest.getNotifyParty())).
                        build()
                ).
                shipmentType(customerBookingRequest.getCargoType()).
                transportMode(customerBookingRequest.getTransportType()).
                direction(customerBookingRequest.getDirection()).
                jobType("STD").
                incoterms(customerBookingRequest.getIncoTerms()).
                serviceType(customerBookingRequest.getServiceMode()).
                status(4).
                fmcTlcId(customerBookingRequest.getFmcTlcId()).
                clientCountry(customerBookingRequest.getClientCountry()).
                consignorCountry(customerBookingRequest.getConsignorCountry()).
                consigneeCountry(customerBookingRequest.getConsigneeCountry()).
                notifyPartyCountry(customerBookingRequest.getNotifyPartyCountry()).
                salesBranch(customerBookingRequest.getSalesBranch()).
                primarySalesAgentEmail(customerBookingRequest.getPrimarySalesAgentEmail()).
                secondarySalesAgentEmail(customerBookingRequest.getSecondarySalesAgentEmail()).
                containersList(consolidationDetails != null && consolidationDetails.size() > 0 ? containerList : null).
                packingList(customerBookingRequest.getPackingList() != null ? customerBookingRequest.getPackingList().stream().map(obj -> {
                    if(!StringUtility.isEmpty(obj.getLengthUnit()))
                    {
                        obj.setWidthUnit(obj.getLengthUnit());
                        obj.setHeightUnit(obj.getLengthUnit());
                    }
                    return obj;
                }).collect(Collectors.toList()) : null).
                fileRepoList(customerBookingRequest.getFileRepoList()).
                routingsList(customerBookingRequest.getRoutingList()).
                consolidationList(customerBookingRequest.getCargoType().equals("FCL") ? consolidationDetails : null).
                notesList(createNotes(notes)).
                sourceTenantId(Long.valueOf(UserContext.getUser().TenantId)).
                source("API").
                bookingType("ONLINE").
                consolRef(consolidationDetails != null && consolidationDetails.size() > 0 ? consolidationDetails.get(0).getReferenceNumber() : "").
                masterBill(consolidationDetails != null && consolidationDetails.size() > 0 ? consolidationDetails.get(0).getBol() : null).
                freightLocalCurrency(UserContext.getUser().CompanyCurrency).
                currentPartyForQuote(customerBookingRequest.getCurrentPartyForQuote()).
                build();

        return this.createFromBooking(CommonRequestModel.buildRequest(shipmentRequest));
    }

    private List<NotesRequest> createNotes(List<Notes> notes){
        if(notes == null) return null;
        return notes.stream().filter(Objects::nonNull).map(note ->
               NotesRequest.builder()
                        .assignedTo(note.getAssignedTo())
                        .label(note.getLabel())
                        .text(note.getText())
                        .insertUserDisplayName(note.getCreatedBy())
                        .isPublic(note.getIsPublic())
                        .insertDate(note.getCreatedAt())
                        .entityType(Constants.CUSTOMER_BOOKING)
                        .build()).toList();
    }

    private PartiesRequest createPartiesRequest(PartiesRequest party)
    {
        if(party == null)
            return null;
        return PartiesRequest.builder()
                .addressCode(party.getAddressCode())
                .addressData(party.getAddressData())
                .orgCode(party.getOrgCode())
                .orgData(party.getOrgData())
                .build();
    }

    private List<PackingRequest> setPackingDetails(List<Containers> containersList, List<PackingRequest> packingRequests, String transportMode, Long consolidationId) {
        if(packingRequests != null && packingRequests.size() > 0) {
            for (PackingRequest packingRequest : packingRequests) {
                if(!IsStringNullOrEmpty(transportMode) && transportMode.equals(Constants.TRANSPORT_MODE_AIR)) {
                    packingRequest.setConsolidationId(consolidationId);
                }
            }
        }
        return packingRequests;
    }

    private List<ContainerRequest> calculateAutoContainerWeightAndVolume(List<ContainerRequest> containersList, List<PackingRequest> packingList) throws RunnerException {
        if(containersList != null && containersList.size() > 0) {
            for (ContainerRequest containers : containersList) {
                if(packingList != null) {
                    List<PackingRequest> packings = packingList.stream().filter(packing -> Objects.equals(packing.getContainerId(), containers.getId())).toList();
                    BigDecimal totalWeight = BigDecimal.ZERO;
                    BigDecimal totalVolume = BigDecimal.ZERO;
                    if(packings != null && packings.size() > 0) {
                        if(IsStringNullOrEmpty(containers.getGrossWeightUnit()))
                            containers.setGrossWeightUnit(Constants.WEIGHT_UNIT_KG);
                        if(IsStringNullOrEmpty(containers.getGrossVolumeUnit()))
                            containers.setGrossVolumeUnit(Constants.VOLUME_UNIT_M3);
                        for (PackingRequest packing : packings) {
                            if(!IsStringNullOrEmpty(packing.getWeightUnit()))
                                totalWeight = totalWeight.add(new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), containers.getGrossWeightUnit()).toString()));
                            if(!IsStringNullOrEmpty(packing.getVolumeUnit()))
                                totalVolume = totalVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), containers.getGrossVolumeUnit()).toString()));
                        }
                        containers.setGrossWeight(totalWeight);
                        containers.setGrossVolume(totalVolume);
                    }
                }
            }
        }
        return containersList;
    }

    public ResponseEntity<IRunnerResponse> calculateAutoUpdateWtVolInShipment(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        try {
            AutoUpdateWtVolRequest request = (AutoUpdateWtVolRequest) commonRequestModel.getData();
            AutoUpdateWtVolResponse response = calculateShipmentWV(request);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateWtVolInShipmentOnChanges(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        try {
            AutoUpdateWtVolRequest request = (AutoUpdateWtVolRequest) commonRequestModel.getData();
            AutoUpdateWtVolResponse response = jsonHelper.convertValue(request, AutoUpdateWtVolResponse.class);
            response = calculateVW(request, response, true);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private AutoUpdateWtVolResponse calculateShipmentWV(AutoUpdateWtVolRequest request) throws RunnerException {
        AutoUpdateWtVolResponse response = jsonHelper.convertValue(request, AutoUpdateWtVolResponse.class);
        List<Packing> packingList = new ArrayList<>();
        if(request.getPackingList() != null)
            packingList = jsonHelper.convertValueToList(request.getPackingList(), Packing.class);
        List<Containers> containersList = new ArrayList<>();
        if(request.getContainersList() != null)
            containersList = jsonHelper.convertValueToList(request.getContainersList(), Containers.class);
//        if(request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
//            response = calculatePacksAndPacksUnit(packingList, response);
//        }
        response = calculatePacksAndPacksUnit(packingList, response);
        response = calculateWeightAndVolumeUnit(request, packingList, response);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        boolean isPacksPresent = packingList != null && packingList.size() > 0;
        if(!isPacksPresent)
            response = updateShipmentDetails(response, containersList);
        response = calculateVW(request, response, true);
        if(shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer().booleanValue()
                || Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR) || isPacksPresent) {
            ShipmentMeasurementDetailsDto dto = new ShipmentMeasurementDetailsDto();
            response.setPackSummary(packingService.calculatePackSummary(packingList, request.getTransportMode(), request.getShipmentType(), dto));
            if(request.getTransportMode() != null && ((Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_SEA)
            && request.getShipmentType() != null && request.getShipmentType().equals(Constants.SHIPMENT_TYPE_LCL))
            || Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR))) {
                response.setInnerPacks(dto.getInnerPacks());
                response.setInnerPackUnit(dto.getInnerPackUnit());
            }
            if(shipmentSettingsDetails.getIsShipmentLevelContainer() != null && shipmentSettingsDetails.getIsShipmentLevelContainer()
            && request.getPackingList() != null && request.getPackingList().size() > 0) {
                response.setWeight(dto.getWeight());
                response.setWeightUnit(dto.getWeightUnit());
                response.setVolume(dto.getVolume());
                response.setVolumeUnit(dto.getVolumeUnit());
                response.setNetWeight(dto.getNetWeight());
                response.setNetWeightUnit(dto.getNetWeightUnit());
                response.setNoOfPacks(dto.getNoOfPacks());
                response.setPacksUnit(dto.getPacksUnit());
            }
            else if(shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer()){
                response.setNoOfPacks(dto.getNoOfPacks());
                response.setPacksUnit(dto.getPacksUnit());
            }
        }
        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        if(Boolean.TRUE.equals(v1TenantSettingsResponse.getP100Branch()) && Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_SEA)) {
            response = calculatePacksAndPacksUnitFromContainer(response, containersList);
        }
        return response;
    }

    private AutoUpdateWtVolResponse updateShipmentDetails(AutoUpdateWtVolResponse response, List<Containers> containersList) throws RunnerException { // to account for updateShipmentDetails flag in v1 container summary
        double totalWeight = 0;
        double packageCount = 0;
        double tareWeight = 0;
        double totalVolume = 0;
        double totalContainerCount = 0;
        int totalPacks = 0;
        String packsUnit = "";
        String toWeightUnit = Constants.WEIGHT_UNIT_KG;
        String toVolumeUnit = Constants.VOLUME_UNIT_M3;
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        if(!IsStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()))
            toWeightUnit = shipmentSettingsDetails.getWeightChargeableUnit();
        if(!IsStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()))
            toVolumeUnit = shipmentSettingsDetails.getVolumeChargeableUnit();
        if(containersList != null) {
            for (Containers containers : containersList) {
                double wInDef = convertUnit(Constants.MASS, containers.getGrossWeight(), containers.getGrossWeightUnit(), toWeightUnit).doubleValue();
                double tarDef = convertUnit(Constants.MASS, containers.getTareWeight(), containers.getTareWeightUnit(), toWeightUnit).doubleValue();
                double volume = convertUnit(Constants.VOLUME, containers.getGrossVolume(), containers.getGrossVolumeUnit(), toVolumeUnit).doubleValue();
                totalWeight = totalWeight + wInDef;
                tareWeight = tareWeight + tarDef;
                double noOfPackages = 0;
                if(containers.getNoOfPackages() != null)
                    noOfPackages = containers.getNoOfPackages().doubleValue();
                if(!IsStringNullOrEmpty(containers.getPacks()))
                    packageCount = packageCount + Long.parseLong(containers.getPacks());
                else
                    packageCount = packageCount + noOfPackages;
                totalVolume = totalVolume + volume;
                if(containers.getContainerCount() != null)
                    totalContainerCount = totalContainerCount + containers.getContainerCount();
                if(!IsStringNullOrEmpty(containers.getPacks()))
                    totalPacks = totalPacks + Integer.parseInt(containers.getPacks());
            }
        }
        if (containersList.size() > 0 ) {
            packsUnit = setPacksUnit(containersList, packsUnit);
        }
        response.setWeight(new BigDecimal(totalWeight));
        response.setVolume(new BigDecimal(totalVolume));
        response.setWeightUnit(toWeightUnit);
        response.setVolumeUnit(toVolumeUnit);
        response.setNoOfPacks(totalPacks == 0 ? null : String.valueOf(totalPacks));
        response.setPacksUnit(packsUnit);
        response.setNetWeight(new BigDecimal(tareWeight));
        response.setNetWeightUnit(toWeightUnit);
        return response;
    }

    private AutoUpdateWtVolResponse calculatePacksAndPacksUnitFromContainer(AutoUpdateWtVolResponse response, List<Containers> containersList) {
        if(containersList != null && containersList.size() > 0) {
            String packsUnit = "";
            long packageCount = 0;
            long totalPacks = 0;
            for (Containers container : containersList) {
                if (!IsStringNullOrEmpty(container.getPacks())) {
                    packageCount = packageCount + Integer.parseInt(container.getPacks());
                } else {
                    if(container.getNoOfPackages() != null)
                        packageCount = packageCount + container.getNoOfPackages();
                }
                if (!IsStringNullOrEmpty(container.getPacks())) {
                    totalPacks = totalPacks + Integer.parseInt(container.getPacks());
                }
            };
            packsUnit = setPacksUnit(containersList, packsUnit);
            response.setNoOfPacks(totalPacks == 0 ? null : String.valueOf(totalPacks));
            response.setPacksUnit(packsUnit);
        }
        return response;
    }

    private String setPacksUnit(List<Containers> containersList, String packsUnit) {
        String firstPacksType = containersList.get(0).getPacksType();
        boolean isSame = containersList.stream()
                .map(Containers::getPacksType)
                .allMatch(packsType -> packsType == null || packsType.equals(firstPacksType));

        if (isSame) {
            packsUnit = firstPacksType;
        } else {
            packsUnit = Constants.MPK;
        }
        return packsUnit;
    }

    private AutoUpdateWtVolResponse calculateWeightAndVolumeUnit(AutoUpdateWtVolRequest request, List<Packing> packings, AutoUpdateWtVolResponse response) throws RunnerException {
        BigDecimal totalWeight = BigDecimal.ZERO;
        BigDecimal totalVolume = BigDecimal.ZERO;
        if(IsStringNullOrEmpty(request.getWeightUnit()))
            response.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        if(IsStringNullOrEmpty(request.getVolumeUnit()))
            response.setVolumeUnit(Constants.VOLUME_UNIT_M3);
        if(packings != null && packings.size() > 0) {
            for (Packing packing : packings) {
                if(packing.getWeight() != null && !IsStringNullOrEmpty(packing.getWeightUnit())) {
                    totalWeight = totalWeight.add(new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), response.getWeightUnit()).toString()));
                }
                if(packing.getVolume() != null && !IsStringNullOrEmpty(packing.getVolumeUnit())) {
                    totalVolume = totalVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), response.getVolumeUnit()).toString()));
                }
            }
            response.setWeight(totalWeight);
            response.setVolume(totalVolume);
            response = calculateVW(request, response, false);
        }
        return response;
    }

    private AutoUpdateWtVolResponse calculateVW(AutoUpdateWtVolRequest request, AutoUpdateWtVolResponse response, boolean recalculateVwObInKgAndM3) throws RunnerException{
        if(IsStringNullOrEmpty(request.getTransportMode()))
            return response;
        if(!IsStringNullOrEmpty(response.getWeightUnit()) && !IsStringNullOrEmpty(response.getVolumeUnit())) {
            VolumeWeightChargeable vwOb = consolidationService.calculateVolumeWeight(request.getTransportMode(), response.getWeightUnit(), response.getVolumeUnit(), response.getWeight(), response.getVolume());
            response.setChargable(vwOb.getChargeable());
            if(request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                response.setChargable(new BigDecimal(roundOffAirShipment(response.getChargable().doubleValue())));
            }
            response.setChargeableUnit(vwOb.getChargeableUnit());
            if(request.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                if(!IsStringNullOrEmpty(request.getShipmentType()) && request.getShipmentType().equals(Constants.SHIPMENT_TYPE_LCL)) {
                    double volInM3 = convertUnit(Constants.VOLUME, response.getVolume(), response.getVolumeUnit(), Constants.VOLUME_UNIT_M3).doubleValue();
                    double wtInKg = convertUnit(Constants.MASS, response.getWeight(), response.getWeightUnit(), Constants.WEIGHT_UNIT_KG).doubleValue();
                    response.setChargable(new BigDecimal(Math.max(wtInKg/1000, volInM3)));
                    response.setChargeableUnit(Constants.VOLUME_UNIT_M3);
                    if(recalculateVwObInKgAndM3)
                        vwOb = consolidationService.calculateVolumeWeight(request.getTransportMode(), Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, new BigDecimal(wtInKg), new BigDecimal(volInM3));
                }
            }
            response.setVolumetricWeight(vwOb.getVolumeWeight());
            response.setVolumetricWeightUnit(vwOb.getVolumeWeightUnit());
        }
        return response;
    }

    private double roundOffAirShipment(double charge) {
        if (charge - 0.50 <= Math.floor(charge) && charge != Math.floor(charge)) {
            charge = Math.floor(charge) + 0.5;
        } else {
            charge = Math.ceil(charge);
        }
        return charge;
    }

    private AutoUpdateWtVolResponse calculatePacksAndPacksUnit(List<Packing> packings, AutoUpdateWtVolResponse response) {
        Integer totalPacks = 0;
        String tempPackingUnit = null;
        String packingUnit = null;
        if(packings != null && packings.size() > 0) {
            for (Packing packing : packings) {
                if(!IsStringNullOrEmpty(packing.getPacks()))
                    totalPacks = totalPacks + Integer.parseInt(packing.getPacks());
                if (tempPackingUnit == null) {
                    tempPackingUnit = packing.getPacksType();
                    packingUnit = packing.getPacksType();
                }
                else {
                    if(!IsStringNullOrEmpty(packing.getPacksType()) && tempPackingUnit.equals(packing.getPacksType())) {
                        packingUnit = Constants.MPK;
                    }
                }
            }
        }
        response.setNoOfPacks(totalPacks.toString());
        response.setPacksUnit(packingUnit);
        return response;
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Shipment update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
//        if (request.getId() == null) {
//            log.error("Request Id is null for Shipment update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
//        }
        // TODO- implement Validation logic

        Optional<ShipmentDetails>oldEntity =retrieveByIdOrGuid(request);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        ShipmentDetails entity = objectMapper.convertValue(request, ShipmentDetails.class);
        entity.setId(oldEntity.get().getId());
        if(entity.getGuid() != null && !oldEntity.get().getGuid().equals(entity.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        if (entity.getContainersList() == null)
            entity.setContainersList(oldEntity.get().getContainersList());
        // update Ata/Atd in shipment from events
        eventService.updateAtaAtdInShipment(entity.getEventsList(), entity, shipmentSettingsDetails);
        entity = shipmentDao.update(entity, false);
        pushShipmentDataToDependentService(entity, false);
        try {
            shipmentSync.sync(entity, null, null, entity.getGuid().toString(), false);
        } catch (Exception e){
            log.error(SyncingConstants.ERROR_SYNCING_SHIPMENTS, e);
        }
        return ResponseHelper.buildSuccessResponse(objectMapper.convertValue(entity, ShipmentDetailsResponse.class));
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> completeUpdate(CommonRequestModel commonRequestModel) throws RunnerException {

        ShipmentRequest shipmentRequest = (ShipmentRequest) commonRequestModel.getData();

        // TODO- implement Validation logic

        Optional<ShipmentDetails> oldEntity = retrieveByIdOrGuid(shipmentRequest);
        long id=oldEntity.get().getId();
        Integer previousStatus = oldEntity.get().getStatus();
        if (!oldEntity.isPresent()) {
            log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_ID_ERROR, shipmentRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try {
            ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
            ShipmentDetails entity = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
            entity.setId(oldEntity.get().getId());

            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());

            boolean syncConsole = beforeSave(entity, oldEntity.get(), false, shipmentRequest, shipmentSettingsDetails);

            entity = shipmentDao.update(entity, false);


            try {
                // audit logs
               auditLogService.addAuditLog(
                       AuditLogMetaData.builder()
                               .newData(entity)
                               .prevData(jsonHelper.readFromJson(oldEntityJsonString, ShipmentDetails.class))
                               .parent(ShipmentDetails.class.getSimpleName())
                               .parentId(entity.getId())
                               .operation(DBOperationType.UPDATE.name()).build()
               );
            }
            catch (Exception e) {
                log.error("Error creating audit service log", e);
            }

            afterSave(entity, oldEntity.get(), false, shipmentRequest, shipmentSettingsDetails, syncConsole);

            ShipmentDetailsResponse response = shipmentDetailsMapper.map(entity);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error("Error occurred due to: " + e.getStackTrace());
            log.error(responseMsg, e);
            throw new ValidationException(e.getMessage());
        }
    }


    private void syncShipment(ShipmentDetails shipmentDetails, Hbl hbl, List<UUID> deletedContGuids, List<Packing> packsForSync, ConsolidationDetails consolidationDetails, boolean syncConsole) {
        String transactionId = shipmentDetails.getGuid().toString();
        try {
            shipmentSync.sync(shipmentDetails, deletedContGuids, null, transactionId, false);
        } catch (Exception e){
            log.error(SyncingConstants.ERROR_SYNCING_SHIPMENTS, e);
        }
        if(hbl != null) {
            try {
                hblSync.sync(hbl, transactionId);
            }
            catch (Exception e) {
                log.error("Error performing sync on hbl entity, {}", e);
            }
        }
        if(syncConsole && consolidationDetails != null) {
            try {
                consolidationSync.sync(consolidationDetails, transactionId, false);
            } catch (Exception e) {
                log.error("Error performing sync on consol entity, {}", e);
            }
        }
        if(packsForSync != null) {
            try {
                packingsSync.sync(packsForSync, transactionId);
            } catch (Exception e) {
                log.error("Error performing sync on packings list, {}", e);
            }
        }
    }
    private boolean beforeSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, Boolean isCreate, ShipmentRequest shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails) throws RunnerException{
        List<Long> tempConsolIds = new ArrayList<>();
        List<Long> removedConsolIds = new ArrayList<>();
        Long id = !Objects.isNull(oldEntity) ? oldEntity.getId() : null;
        boolean syncConsole = false;

        if(shipmentDetails.getCarrierDetails() != null) {
            if (shipmentDetails.getTransportMode() != null && shipmentDetails.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) {
                shipmentDetails.getCarrierDetails().setVoyage(null);
            } else {
                shipmentDetails.getCarrierDetails().setFlightNumber(null);
            }
        }

        if (Objects.isNull(shipmentDetails.getSourceTenantId()))
            shipmentDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));

        boolean isNewConsolAttached = false;
        List<ConsolidationDetailsRequest> consolidationDetailsRequests = shipmentRequest.getConsolidationList();
        if (consolidationDetailsRequests != null) {
            Set<Long> oldConsolIds = Objects.isNull(oldEntity) ? null : oldEntity.getConsolidationList().stream().map(e -> e.getId()).collect(Collectors.toSet());
            for (ConsolidationDetailsRequest consolidation : consolidationDetailsRequests) {
                if (consolidation.getId() != null) {
                    tempConsolIds.add(consolidation.getId());
                    if(!Objects.isNull(oldConsolIds) && oldConsolIds.contains(consolidation.getId()))
                        oldConsolIds.remove(consolidation.getId());
                }
            }
            removedConsolIds = !Objects.isNull(oldConsolIds) ? oldConsolIds.stream().toList() : new ArrayList<>();

            if(!consolidationDetailsRequests.isEmpty() && (oldEntity == null || oldEntity.getConsolidationList() == null ||  oldEntity.getConsolidationList().size() == 0 || removedConsolIds.size() > 0)) {
                isNewConsolAttached = true;
            }
        }
        else
            tempConsolIds = Objects.isNull(oldEntity) ? new ArrayList<>() : oldEntity.getConsolidationList().stream().map(e -> e.getId()).toList();

        List<PackingRequest> packingRequest = shipmentRequest.getPackingList();
        List<ContainerRequest> containerRequest = shipmentRequest.getContainersList();

        if(removedConsolIds != null && removedConsolIds.size() > 0) {
            List<Containers> allConsolConts = new ArrayList<>();
            for(Long consolidationId: removedConsolIds) {
                List<Containers> containersList = containerDao.findByConsolidationId(consolidationId);
                if(containersList != null && containersList.size() > 0) {
                    allConsolConts.addAll(containersList);
                }
            }
            if(allConsolConts.size() > 0) {
                if(Objects.isNull(containerRequest) && !Objects.isNull(oldEntity))
                    containerRequest = jsonHelper.convertValueToList(oldEntity.getContainersList(), ContainerRequest.class);
                containerRequest.removeIf(obj2 -> allConsolConts.stream().anyMatch(obj1 -> obj1.getId().equals(obj2.getId())));
            }
        }

        if(shipmentDetails.getContainerAutoWeightVolumeUpdate() != null && shipmentDetails.getContainerAutoWeightVolumeUpdate().booleanValue() && packingRequest != null) {
            if(Objects.isNull(containerRequest) && !Objects.isNull(oldEntity))
                containerRequest = jsonHelper.convertValueToList(oldEntity.getContainersList(), ContainerRequest.class);
            containerRequest = calculateAutoContainerWeightAndVolume(containerRequest, packingRequest);
        }
        Long consolidationId = null;
        if(shipmentDetails.getConsolidationList() != null && shipmentDetails.getConsolidationList().size() > 0)
            consolidationId = shipmentDetails.getConsolidationList().get(0).getId();
        List<Containers> updatedContainers = new ArrayList<>();

        if (containerRequest != null) {
            for (ContainerRequest containerRequest1 : containerRequest) {
                containerRequest1.setConsolidationId(consolidationId);
            }
            updatedContainers = containerDao.updateEntityFromShipmentConsole(commonUtils.convertToEntityList(containerRequest, Containers.class, isCreate), consolidationId, id, false);
        } else if (!Objects.isNull(oldEntity)){
            updatedContainers = oldEntity.getContainersList();
        }
        shipmentDetails.setContainersList(updatedContainers);
        ConsolidationDetails consolidationDetails = null;

        if(updatedContainers.size() > 0 || (shipmentRequest.getAutoCreateConsole() != null  && shipmentRequest.getAutoCreateConsole())) {
            if((tempConsolIds == null || tempConsolIds.size() == 0) && (shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer())) {
                consolidationDetails = createConsolidation(shipmentDetails, updatedContainers);
                if(!Objects.isNull(consolidationDetails)) {
                    shipmentDetails.setConsolidationList(new ArrayList<>(Arrays.asList(consolidationDetails)));
                    if(IsStringNullOrEmpty(shipmentDetails.getMasterBill()))
                        shipmentDetails.setMasterBill(consolidationDetails.getBol());
                    syncConsole = true;
                }
            }
        }
        validateBeforeSave(shipmentDetails);


        if(isNewConsolAttached) {
            ConsolidationDetails consolidationDetails1 = shipmentDetails.getConsolidationList().get(0);
            shipmentDetails.setMasterBill(consolidationDetails1.getBol());
            shipmentDetails.setDirection(consolidationDetails1.getShipmentType());
            if (shipmentDetails.getCarrierDetails() == null) {
                shipmentDetails.setCarrierDetails(new CarrierDetails());
            }
            if (consolidationDetails1.getCarrierDetails() != null) {
                shipmentDetails.getCarrierDetails().setVoyage(consolidationDetails1.getCarrierDetails().getVoyage());
                shipmentDetails.getCarrierDetails().setVessel(consolidationDetails1.getCarrierDetails().getVessel());
                shipmentDetails.getCarrierDetails().setShippingLine(consolidationDetails1.getCarrierDetails().getShippingLine());
                shipmentDetails.getCarrierDetails().setAircraftType(consolidationDetails1.getCarrierDetails().getAircraftType());
            }
            var console = shipmentDetails.getConsolidationList().get(0);
            List<Awb> awb = awbDao.findByConsolidationId(console.getId());
            if(awb != null && !awb.isEmpty() && awb.get(0).getAirMessageStatus() != null && (Objects.equals(awb.get(0).getAirMessageStatus(), AwbStatus.AIR_MESSAGE_SENT) ||
                    Objects.equals(awb.get(0).getAirMessageStatus(), AwbStatus.AIR_MESSAGE_FAILED) || Objects.equals(awb.get(0).getAirMessageStatus(), AwbStatus.AIR_MESSAGE_SUCCESS))) {
                throw new RunnerException("FWB & FZB are already submitted and further modifications are prohibited for given console.");
            }
        }

        if(!isCreate){
            consolidationDetails = updateLinkedShipmentData(shipmentDetails, oldEntity);
            if(!Objects.isNull(consolidationDetails)) {
                shipmentDetails.setConsolidationList(new ArrayList<>(Arrays.asList(consolidationDetails)));
                syncConsole = true;
            }
        }

        if(shipmentDetails.getReceivingBranch() != null && shipmentDetails.getReceivingBranch() == 0)
            shipmentDetails.setReceivingBranch(null);
        if(shipmentDetails.getTriangulationPartner() != null && shipmentDetails.getTriangulationPartner() == 0)
            shipmentDetails.setTriangulationPartner(null);
        if(shipmentDetails.getDocumentationPartner() != null && shipmentDetails.getDocumentationPartner() == 0)
            shipmentDetails.setDocumentationPartner(null);

        return syncConsole;
    }

    private void validateBeforeSave(ShipmentDetails shipmentDetails) throws RunnerException {
        if(shipmentDetails.getConsignee() != null && shipmentDetails.getConsigner() != null)
        {
            if(shipmentDetails.getConsignee().getOrgCode() != null && shipmentDetails.getConsigner().getOrgCode() != null && shipmentDetails.getConsigner().getOrgCode().equals(shipmentDetails.getConsignee().getOrgCode()))
                throw new ValidationException("Consignor & Consignee parties can't be selected as same.");
        }
        if(!IsStringNullOrEmpty(shipmentDetails.getJobType()) && shipmentDetails.getJobType().equals(Constants.SHIPMENT_TYPE_DRT)){
            if(!IsStringNullOrEmpty(shipmentDetails.getTransportMode()) && !shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && !shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
                shipmentDetails.setHouseBill(shipmentDetails.getMasterBill());
            }
            else if(!IsStringNullOrEmpty(shipmentDetails.getTransportMode()) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) ||
                    shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA))) {
                shipmentDetails.setHouseBill(null);
            }
        }
//        Credit Limit check while shipment creation is removed for now
//        v1ServiceUtil.validateCreditLimit(shipmentDetails.getClient(), ShipmentConstants.SHIPMENT_CREATION, shipmentDetails.getGuid(), false);

        if(!Objects.isNull(shipmentDetails.getConsolidationList()) && !shipmentDetails.getConsolidationList().isEmpty()) {
            ConsolidationDetails console = shipmentDetails.getConsolidationList().get(0);
            ConsolidationDetails tempConsole = new ConsolidationDetails();
            tempConsole.setId(console.getId());
            if(console.equals(tempConsole)){
                console = consolidationDetailsDao.findById(console.getId()).get();
                shipmentDetails.setConsolidationList(new ArrayList<>(Arrays.asList(console)));
            }
            shipmentDetails.setConsolRef(shipmentDetails.getConsolidationList().get(0).getReferenceNumber());
        }

        Parties consignor = shipmentDetails.getConsigner();
        if(Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
            List<Parties> orgList = new ArrayList<>();
            if(consignor != null) {
                if(consignor != null && StringUtility.isNotEmpty(consignor.getAddressCode())) {
                    orgList.add(consignor);
                }
            }

            if(shipmentDetails.getId() == null && shipmentDetails.getAdditionalDetails() != null) {
                if(shipmentDetails.getAdditionalDetails().getImportBroker() != null || shipmentDetails.getAdditionalDetails().getExportBroker() != null) {
                    if(shipmentDetails.getAdditionalDetails().getImportBroker() != null && StringUtility.isNotEmpty(shipmentDetails.getAdditionalDetails().getImportBroker().getAddressCode())) {
                        orgList.add(shipmentDetails.getAdditionalDetails().getImportBroker());
                    }
                    if(shipmentDetails.getAdditionalDetails().getExportBroker() != null && StringUtility.isNotEmpty(shipmentDetails.getAdditionalDetails().getExportBroker().getAddressCode())) {
                        orgList.add(shipmentDetails.getAdditionalDetails().getExportBroker());
                    }
                }
            }

            if(orgList.size() > 0) {
                OrgAddressResponse orgAddressResponse = v1ServiceUtil.fetchOrgInfoFromV1(orgList);
                if (orgAddressResponse != null) {
                    Map<String, Map<String, Object>> addressMap = orgAddressResponse.getAddresses();
                    if(consignor != null) {
                        if (addressMap.containsKey(consignor.getOrgCode() + "#" + consignor.getAddressCode())) {
                            Map<String, Object> addressConsignorAgent = addressMap.get(consignor.getOrgCode() + "#" + consignor.getAddressCode());
                            if (addressConsignorAgent.containsKey(Constants.RA_KC_TYPE)) {
                                var rakcType = addressConsignorAgent.get(Constants.RA_KC_TYPE);
                                if (rakcType != null && (Integer) rakcType == RAKCType.KNOWN_CONSIGNOR.getId() && (shipmentDetails.getAdditionalDetails().getScreeningStatus() == null ||
                                        shipmentDetails.getAdditionalDetails().getScreeningStatus().isEmpty() ||
                                        shipmentDetails.getSecurityStatus() == null)) {
                                    throw new RunnerException("Screening Status and Security Status is mandatory for KC consginor.");
                                }
                            }
                        }
                    }

                    if(shipmentDetails.getId() == null && shipmentDetails.getAdditionalDetails() != null) {
                        if (shipmentDetails.getAdditionalDetails().getImportBroker() != null && StringUtility.isNotEmpty(shipmentDetails.getAdditionalDetails().getImportBroker().getAddressCode())) {
                            if (!checkRaStatusFields(shipmentDetails, orgAddressResponse, shipmentDetails.getAdditionalDetails().getImportBroker())) {
                                throw new RunnerException("Screening Status and Security Status is mandatory for RA Origin Agent.");
                            }
                        }

                        if (shipmentDetails.getAdditionalDetails().getExportBroker() != null && StringUtility.isNotEmpty(shipmentDetails.getAdditionalDetails().getExportBroker().getAddressCode())) {
                            if (!checkRaStatusFields(shipmentDetails, orgAddressResponse, shipmentDetails.getAdditionalDetails().getExportBroker())) {
                                throw new RunnerException("Screening Status and Security Status is mandatory for RA Destination Agent.");
                            }
                        }
                    }
                }
            }
        }
    }

    public boolean checkRaStatusFields(ShipmentDetails shipmentDetails, OrgAddressResponse orgAddressResponse, Parties parties) {
        Map<String, Map<String, Object>> addressMap = orgAddressResponse.getAddresses();
        if (addressMap.containsKey(parties.getOrgCode() + "#" + parties.getAddressCode())) {
            Map<String, Object> addressConsignorAgent = addressMap.get(parties.getOrgCode() + "#" + parties.getAddressCode());
            if (addressConsignorAgent.containsKey(Constants.RA_KC_TYPE)) {
                var rakcType = addressConsignorAgent.get(Constants.RA_KC_TYPE);
                if (rakcType != null && (Integer)rakcType == RAKCType.REGULATED_AGENT.getId() && (shipmentDetails.getAdditionalDetails().getScreeningStatus() == null ||
                        shipmentDetails.getAdditionalDetails().getScreeningStatus().isEmpty() ||
                        shipmentDetails.getSecurityStatus() == null)) {
                    return false;
                }
            }
        }
        return true;
    }

    public void afterSave(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, Boolean isCreate, ShipmentRequest shipmentRequest, ShipmentSettingsDetails shipmentSettingsDetails, boolean syncConsole) throws RunnerException {
        List<BookingCarriageRequest> bookingCarriageRequestList = shipmentRequest.getBookingCarriagesList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = shipmentRequest.getTruckDriverDetails();
        List<PackingRequest> packingRequestList = shipmentRequest.getPackingList();
        AdditionalDetailRequest additionalDetailRequest = shipmentRequest.getAdditionalDetails();
        List<ContainerRequest> containerRequestList = shipmentRequest.getContainersList();
        List<ELDetailsRequest> elDetailsRequestList = shipmentRequest.getElDetailsList();
        List<EventsRequest> eventsRequestList = shipmentRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = shipmentRequest.getFileRepoList();
        List<JobRequest> jobRequestList = shipmentRequest.getJobsList();
        List<NotesRequest> notesRequestList = shipmentRequest.getNotesList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = shipmentRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = shipmentRequest.getRoutingsList();
        List<ServiceDetailsRequest> serviceDetailsRequestList = shipmentRequest.getServicesList();
        List<PartiesRequest> shipmentAddressList = shipmentRequest.getShipmentAddresses();
        CarrierDetailRequest carrierDetailRequest = shipmentRequest.getCarrierDetails();

        Long id = shipmentDetails.getId();
        Long consolidationId = null;
        if(shipmentDetails.getConsolidationList() != null && shipmentDetails.getConsolidationList().size() > 0)
            consolidationId = shipmentDetails.getConsolidationList().get(0).getId();
        Integer previousStatus = !Objects.isNull(oldEntity) ? oldEntity.getStatus() : null;

        List<Containers> updatedContainers = shipmentDetails.getContainersList();
        List<Packing> updatedPackings = new ArrayList<>();
        List<Long> deleteContainerIds = new ArrayList<>();
        List<Packing> packsForSync = null;
        List<UUID> deletedContGuids = new ArrayList<>();

        if(!isCreate){
            if(shipmentRequest.getDeletedContainerIds() != null && shipmentRequest.getDeletedContainerIds().size() > 0) {
                deleteContainerIds = shipmentRequest.getDeletedContainerIds().stream().filter(e -> e.getId() != null).map(e -> e.getId()).toList();
                if(deleteContainerIds != null && deleteContainerIds.size() > 0) {
                    ListCommonRequest listCommonRequest = constructListCommonRequest("containerId", deleteContainerIds, "IN");
                    Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                    Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
                    if(packings != null && packings.getContent() != null && !packings.getContent().isEmpty()) {
                        List<Packing> packingList = new ArrayList<>();
                        for (Packing packing : packings.getContent()) {
                            packing.setContainerId(null);
                            packingList.add(packing);
                        }
                        packingDao.saveAll(packingList);
                        packsForSync = packingList;
                    }
                    listCommonRequest = constructListCommonRequest("id", deleteContainerIds, "IN");
                    Pair<Specification<Containers>, Pageable> pair2 = fetchData(listCommonRequest, Containers.class);
                    Page<Containers> containersPage = containerDao.findAll(pair2.getLeft(), pair2.getRight());
                    if(containersPage != null && !containersPage.isEmpty())
                        deletedContGuids = containersPage.stream().map(e -> e.getGuid()).toList();
                    for (Long containerId : deleteContainerIds) {
                        containerDao.deleteById(containerId);
                    }
                }
            }
        }

        if (bookingCarriageRequestList != null) {
            List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(commonUtils.convertToEntityList(bookingCarriageRequestList, BookingCarriage.class, isCreate), id);
            shipmentDetails.setBookingCarriagesList(updatedBookingCarriages);
        }
        if (truckDriverDetailsRequestList != null) {
            List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(truckDriverDetailsRequestList, TruckDriverDetails.class, isCreate), id);
            shipmentDetails.setTruckDriverDetails(updatedTruckDriverDetails);
        }

        if (packingRequestList != null) {
            if(Objects.isNull(updatedContainers) && !Objects.isNull(oldEntity))
                updatedContainers = oldEntity.getContainersList();
            packingRequestList = setPackingDetails(updatedContainers, packingRequestList, shipmentDetails.getTransportMode(), consolidationId);
            updatedPackings = packingDao.updateEntityFromShipment(commonUtils.convertToEntityList(packingRequestList, Packing.class, isCreate), id, deleteContainerIds);
            shipmentDetails.setPackingList(updatedPackings);
        }
        if (elDetailsRequestList != null) {
            List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(elDetailsRequestList, ELDetails.class, isCreate), id);
            shipmentDetails.setElDetailsList(updatedELDetails);
        }
        if (eventsRequestList != null) {
            List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(eventsRequestList, Events.class, isCreate), id, Constants.SHIPMENT);
            shipmentDetails.setEventsList(updatedEvents);
            eventService.updateAtaAtdInShipment(updatedEvents, shipmentDetails, shipmentSettingsDetails);
        }
        if(isCreate){
            // create Shipment event on the bases of auto create event flag
            if(shipmentSettingsDetails.getAutoEventCreate() != null && shipmentSettingsDetails.getAutoEventCreate())
                autoGenerateCreateEvent(shipmentDetails);
            ConsolidationDetails consolidationDetails = updateLinkedShipmentData(shipmentDetails, null);
            if(!Objects.isNull(consolidationDetails)) {
                shipmentDetails.setConsolidationList(new ArrayList<>(Arrays.asList(consolidationDetails)));
                syncConsole = true;
            }
        }
        // Create events on basis of shipment status Confirmed/Created
        autoGenerateEvents(shipmentDetails, previousStatus);

        if (referenceNumbersRequestList != null) {
            List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(commonUtils.convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class, isCreate), id);
            shipmentDetails.setReferenceNumbersList(updatedReferenceNumbers);
        }
        if (routingsRequestList != null) {
            List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(commonUtils.convertToEntityList(routingsRequestList, Routings.class, isCreate), id);
            shipmentDetails.setRoutingsList(updatedRoutings);
        }
        if (serviceDetailsRequestList != null) {
            List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(commonUtils.convertToEntityList(serviceDetailsRequestList, ServiceDetails.class, isCreate), id);
            shipmentDetails.setServicesList(updatedServiceDetails);
        }
        if (notesRequestList != null) {
            List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(notesRequestList, Notes.class, isCreate), id, Constants.SHIPMENT);
            shipmentDetails.setNotesList(updatedNotes);
        }

        if (shipmentAddressList != null) {
            List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(shipmentAddressList, Parties.class, isCreate), id, Constants.SHIPMENT_ADDRESSES);
            shipmentDetails.setShipmentAddresses(updatedParties);
        }

        // Create Shipment Route in Console for Auto Attach Consolidation;
        if (shipmentRequest.getReplaceConsoleRoute() != null && shipmentRequest.getReplaceConsoleRoute()){
            createShipmentRouteInConsole(shipmentRequest);
        }
        Hbl hbl = null;
        if(updatedContainers.size() > 0) {
            hbl = hblService.checkAllContainerAssigned(shipmentDetails, updatedContainers, updatedPackings);
        }
        pushShipmentDataToDependentService(shipmentDetails, isCreate);

        ConsolidationDetails consolidationDetails = null;
        if(!Objects.isNull(shipmentDetails.getConsolidationList()) && !shipmentDetails.getConsolidationList().isEmpty()){
            consolidationDetails = shipmentDetails.getConsolidationList().get(0);
        }
        // Syncing shipment to V1
        syncShipment(shipmentDetails, hbl, deletedContGuids, packsForSync, consolidationDetails, syncConsole);
        if (TenantSettingsDetailsContext.getCurrentTenantSettings().getP100Branch() != null && TenantSettingsDetailsContext.getCurrentTenantSettings().getP100Branch())
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> bookingIntegrationsUtility.updateBookingInPlatform(shipmentDetails)), executorService);
    }

    public void pushShipmentDataToDependentService(ShipmentDetails shipmentDetails, boolean isCreate) {
        try {
            if(shipmentDetails.getTenantId() == null)
                shipmentDetails.setTenantId(TenantContext.getCurrentTenant());
            if (IsStringNullOrEmpty(shipmentDetails.getUpdatedBy()))
                shipmentDetails.setUpdatedBy(UserContext.getUser().getUsername());
            KafkaResponse kafkaResponse = producer.getKafkaResponse(shipmentDetails, isCreate);
            log.info("Producing shipment data to kafka with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(kafkaResponse));
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, UUID.randomUUID().toString());
        }
        catch (Exception e) {
            log.error("Error Producing shipment to kafka, error is due to " + e.getMessage());
        }
        try {
            if(shipmentDetails.getStatus() != null && !Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Completed.getValue()) || shipmentDetails.getStatus() != null && !Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Cancelled.getValue())) {
                if (trackingServiceAdapter.checkIfConsolAttached(shipmentDetails)|| (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getShipmentType().equals(Constants.SHIPMENT_TYPE_DRT) && !Objects.isNull(shipmentDetails.getHouseBill()))) {
                    UniversalTrackingPayload _utPayload = trackingServiceAdapter.mapShipmentDataToTrackingServiceData(shipmentDetails);
                    List<UniversalTrackingPayload> trackingPayloads = new ArrayList<>();
                    if(_utPayload != null) {
                        trackingPayloads.add(_utPayload);
                        var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                        log.info("Producing tracking service payload from shipment with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonBody);
                        trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody, false);
                    }
                }
            }
            if(shipmentDetails.getSource() != null && shipmentDetails.getSource().equals(Constants.API)) {
                var events = trackingServiceAdapter.getAllEvents(shipmentDetails,null, shipmentDetails.getBookingReference());
                var universalEventsPayload = trackingServiceAdapter.mapEventDetailsForTracking(shipmentDetails.getBookingReference(),Constants.SHIPMENT, shipmentDetails.getShipmentId(), events);
                List<UniversalTrackingPayload.UniversalEventsPayload> trackingPayloads= new ArrayList<>();
                if(universalEventsPayload != null) {
                    trackingPayloads.add(universalEventsPayload);
                    var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                    log.info("Producing tracking service payload from shipment with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonBody);
                    trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody,true);
                }
            }
        }
        catch (Exception e) {
            log.error(e.getMessage());
        }
    }

    private void createShipmentRouteInConsole (ShipmentRequest shipmentRequest) throws RunnerException{
        List<ConsolidationDetailsRequest> consoleRequest = shipmentRequest.getConsolidationList();
        List<Routings> createRoutes = new ArrayList<>();
        if(!Objects.isNull(shipmentRequest.getRoutingsList())) {
            if(shipmentRequest.getCreateMainLegRoute() != null && shipmentRequest.getCreateMainLegRoute()){
                List<RoutingsRequest> routeRequestList = shipmentRequest.getRoutingsList().stream().sorted(Comparator.comparingLong(RoutingsRequest::getLeg)).toList();
                var routeRequest = routeRequestList.stream().filter(x -> x.getMode().equals(shipmentRequest.getTransportMode())).findFirst();
                if(routeRequest.isPresent()) {
                    createRoutes.add(jsonHelper.convertValue(routeRequest.get(), Routings.class));
                    createRoutes = createConsoleRoutePayload(createRoutes);
                }
            } else {
                createRoutes = convertToEntityList(shipmentRequest.getRoutingsList(), Routings.class);
                createRoutes = createConsoleRoutePayload(createRoutes);
            }
        }
        if(consoleRequest != null && !consoleRequest.isEmpty() && createRoutes != null && !createRoutes.isEmpty()) {
            for (var console : consoleRequest) {
                routingsDao.updateEntityFromConsole(createRoutes, console.getId());
            }
        }
    }

    private List<Routings> createConsoleRoutePayload(List<Routings> routes){
        List<Routings> responseList = new ArrayList<>();
        for (var route : routes){
            Routings routings = new Routings();
            routings.setLeg(1L);
            routings.setPol(route.getPol());
            routings.setPod(route.getPod());
            routings.setMode(route.getMode());
            routings.setEta(route.getEta());
            routings.setEtd(route.getEtd());
            routings.setTransitDays(route.getTransitDays());
            routings.setAta(route.getAta());
            routings.setAtd(route.getAtd());
            routings.setVesselName(route.getVesselName());
            routings.setVoyage(route.getVoyage());
            routings.setCarrier(route.getCarrier());
            routings.setFlightNumber(route.getFlightNumber());
            responseList.add(routings);
        }
        return responseList;
    }

    public ConsolidationDetails createConsolidation(ShipmentDetails shipmentDetails, List<Containers> containers) throws RunnerException {
        ShipmentSettingsDetails shipmentSettings = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        if(shipmentSettings.getShipConsolidationContainerEnabled()) {
            ConsolidationDetails consolidationDetails = new ConsolidationDetails();
            consolidationDetails.setConsolidationType(Constants.SHIPMENT_TYPE_DRT);
            consolidationDetails.setTransportMode(shipmentDetails.getTransportMode());
            if((shipmentSettings.getConsolidationLite() == null || !shipmentSettings.getConsolidationLite())
                    && !Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_ROA)
                    && (StringUtility.isEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) || StringUtility.isEmpty(shipmentDetails.getCarrierDetails().getDestinationPort()))) {
                throw new ValidationException("Not able to create consolidation, before adding 'New Containers' , please provide ‘Origin’ and ‘Destination’ values.");
            }
            if(StringUtility.isNotEmpty(shipmentDetails.getCarrierDetails().getOriginPort()) && Objects.equals(shipmentDetails.getCarrierDetails().getOriginPort(), shipmentDetails.getCarrierDetails().getDestinationPort())) {
                throw new ValidationException("‘Origin’ and ‘Destination’ can't be same");
            }
            consolidationDetails.setCarrierDetails(jsonHelper.convertValue(shipmentDetails.getCarrierDetails(), CarrierDetails.class));
            consolidationDetails.getCarrierDetails().setId(null);
            consolidationDetails.getCarrierDetails().setGuid(null);
            if(shipmentSettings.getShipmentLite() != null && shipmentSettings.getShipmentLite() && shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getDirection().equals(Constants.DIRECTION_EXP)) {
                consolidationDetails.setPayment(shipmentDetails.getPaymentTerms());
            }
            if(consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                consolidationDetails.getCarrierDetails().setOrigin(consolidationDetails.getCarrierDetails().getOriginPort());
                consolidationDetails.getCarrierDetails().setDestination(consolidationDetails.getCarrierDetails().getDestinationPort());
            }
            consolidationDetails.setShipmentType(shipmentDetails.getDirection());
            consolidationDetails.setContainerCategory(shipmentDetails.getShipmentType());
            consolidationDetails.setIsReceivingAgentFreeTextAddress(false);
            consolidationDetails.setIsSendingAgentFreeTextAddress(false);
            consolidationDetails.setIsInland(false);
            consolidationDetails.setCarrierBookingRef(shipmentDetails.getBookingNumber());
            consolidationDetails.setSourceTenantId(TenantContext.getCurrentTenant().longValue());
            if(StringUtility.isNotEmpty(shipmentDetails.getMasterBill())) {
                consolidationDetails.setBol(shipmentDetails.getMasterBill());
            }
            consolidationService.generateConsolidationNumber(consolidationDetails);
            if(consolidationDetails.getShipmentType() != null && !consolidationDetails.getShipmentType().isEmpty()
            && consolidationDetails.getShipmentType().equals(Constants.IMP) || consolidationDetails.getShipmentType().equals(Constants.DIRECTION_EXP)) {
                Parties defaultParty = null;
                try {
                    PartyRequestV2 partyRequestV2 = v1Service.getDefaultOrg();
                    if(!Objects.isNull(partyRequestV2))
                        defaultParty = modelMapper.map(partyRequestV2, Parties.class);
                } catch (Exception ignored) {}
                if(!Objects.isNull(defaultParty)) {
                    if(consolidationDetails.getShipmentType().equals(Constants.DIRECTION_EXP)) {
                        consolidationDetails.setSendingAgent(defaultParty);
                        if(consolidationDetails.getReceivingAgent() != null && consolidationDetails.getReceivingAgent().getOrgCode() != null
                            && consolidationDetails.getReceivingAgent().getOrgCode().equals(defaultParty.getOrgCode()))
                            consolidationDetails.setReceivingAgent(null);
                    }
                    else {
                        consolidationDetails.setReceivingAgent(defaultParty);
                        if(consolidationDetails.getSendingAgent() != null && consolidationDetails.getSendingAgent().getOrgCode() != null
                            && consolidationDetails.getSendingAgent().getOrgCode().equals(defaultParty.getOrgCode()))
                            consolidationDetails.setSendingAgent(null);
                    }
                }
            }
            List<Routings> routings = new ArrayList<>();
            if(shipmentDetails.getRoutingsList() != null && shipmentDetails.getRoutingsList().size() > 0)
                routings = shipmentDetails.getRoutingsList().stream().sorted(Comparator.comparingLong(Routings::getLeg)).toList();
            var routeRequest = routings.stream().filter(x -> x.getMode().equals(shipmentDetails.getTransportMode())).findFirst();
            List<Routings> createRoutes = new ArrayList<>();
            if(routeRequest.isPresent()) {
                createRoutes.add(convertToClass(routeRequest.get(), Routings.class));
                createRoutes = createConsoleRoutePayload(createRoutes);
                consolidationDetails.setRoutingsList(createRoutes);
            }
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false);
            if(createRoutes != null && !createRoutes.isEmpty()) {
                routingsDao.saveEntityFromConsole(createRoutes, consolidationDetails.getId());
            }
            Long id = consolidationDetails.getId();
            if(containers != null && containers.size() > 0) {
                containers = containers.stream().map(e -> e.setConsolidationId(id)).toList();
                containers = containerDao.saveAll(containers);
            }
            consolidationDetails.setContainersList(containers);
            if(shipmentSettings.getAutoEventCreate() != null && shipmentSettings.getAutoEventCreate()) {
                consolidationService.autoGenerateEvents(consolidationDetails);
            }
            consolidationService.pushShipmentDataToDependentService(consolidationDetails, true);
            return consolidationDetails;
        }
        return null;
    }

    @Override
    public void exportExcel(HttpServletResponse response, CommonRequestModel commonRequestModel) throws IOException, IllegalAccessException {
        String responseMsg;

        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        if (request == null) {
            log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
        }
        request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS));
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info(ShipmentConstants.SHIPMENT_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());
        Map<String, Integer> headerMap = new HashMap<>();
        for (int i = 0; i < ShipmentConstants.SHIPMENT_HEADERS.size(); i++) {
            headerMap.put(ShipmentConstants.SHIPMENT_HEADERS.get(i), i);
        }

        try(Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("ShipmentList");
            makeHeadersInSheet(sheet, workbook);

            //Filling the data
            List<IRunnerResponse> shipmentListResponseData = convertEntityListToDtoList(shipmentDetailsPage.getContent());
            for (int i = 0; i < shipmentListResponseData.size(); i++) {
                Row itemRow = sheet.createRow(i + 1);
                ShipmentListResponse shipment = (ShipmentListResponse) shipmentListResponseData.get(i);
                String origin = "", destination = "", destinationPort = "", originPort = "";
                if(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getUnlocationData() != null){
                    origin = shipment.getCarrierDetails().getUnlocationData().get("origin");
                    destination = shipment.getCarrierDetails().getUnlocationData().get("destination");
                    destinationPort = shipment.getCarrierDetails().getUnlocationData().get("destinationPort");
                    originPort = shipment.getCarrierDetails().getUnlocationData().get("originPort");
                }
                if(shipment.getCarrierDetails() != null){
                    origin = StringUtility.isEmpty(origin) ? shipment.getCarrierDetails().getOrigin() : origin;
                    destination = StringUtility.isEmpty(destination) ? shipment.getCarrierDetails().getDestination() : destination;
                    destinationPort = StringUtility.isEmpty(destinationPort) ? shipment.getCarrierDetails().getDestinationPort() : destinationPort;
                    originPort = StringUtility.isEmpty(originPort) ? shipment.getCarrierDetails().getOriginPort() : originPort;
                }
                LocalTimeZoneHelper.transformTimeZone(shipment);
                itemRow.createCell(headerMap.get("Shipment Clone")).setCellValue("");
                itemRow.createCell(headerMap.get("Shipment Number")).setCellValue(shipment.getShipmentId());
                itemRow.createCell(headerMap.get("Order Number")).setCellValue(shipment.getOrderManagementNumber());
                itemRow.createCell(headerMap.get("Status")).setCellValue(String.valueOf(ShipmentStatus.values()[(shipment.getStatus())]));
                itemRow.createCell(headerMap.get("Transport Mode")).setCellValue(shipment.getTransportMode());
                itemRow.createCell(headerMap.get("Bill Status")).setCellValue(shipment.getBillStatus());
                itemRow.createCell(headerMap.get("MBL Number")).setCellValue(shipment.getMasterBill());
                itemRow.createCell(headerMap.get("Incoterm")).setCellValue(shipment.getIncoterms());
                itemRow.createCell(headerMap.get("Service Type")).setCellValue(shipment.getServiceType());
                itemRow.createCell(headerMap.get("Release Type")).setCellValue(Objects.isNull(shipment.getAdditionalDetails()) ? "" : shipment.getAdditionalDetails().getReleaseType());
                itemRow.createCell(headerMap.get("House Bill Type")).setCellValue(shipment.getAdditionalDetails() != null ? shipment.getAdditionalDetails().getHouseBillType() : "");
                itemRow.createCell(headerMap.get("Delivery Mode")).setCellValue(Objects.isNull(shipment.getDeliveryDetails()) ? "" : shipment.getDeliveryDetails().getDropMode());
                itemRow.createCell(headerMap.get("Consolidation Type")).setCellValue(String.valueOf(shipment.getJobType()));
                itemRow.createCell(headerMap.get("Activity Type")).setCellValue(Objects.isNull(shipment.getAdditionalDetails()) ? "" : shipment.getAdditionalDetails().getActivityType());
                itemRow.createCell(headerMap.get("Shipment Type")).setCellValue(shipment.getDirection());
                itemRow.createCell(headerMap.get("Carrier")).setCellValue(Objects.isNull(shipment.getCarrierDetails()) ? "" : shipment.getCarrierDetails().getShippingLine());
                itemRow.createCell(headerMap.get("Vessel Name/Flight")).setCellValue(shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getVessel() : "");
                itemRow.createCell(headerMap.get("Flight Number")).setCellValue(Optional.ofNullable(shipment.getCarrierDetails()).map(c -> c.getFlightNumber()).orElse(""));
                itemRow.createCell(headerMap.get("Voyage/Flight No.")).setCellValue(Objects.isNull(shipment.getCarrierDetails()) ? "" : shipment.getCarrierDetails().getVoyage());
                itemRow.createCell(headerMap.get("Paid Place Name")).setCellValue(shipment.getAdditionalDetails() != null && shipment.getAdditionalDetails().getUnlocationData() != null ? String.valueOf(shipment.getAdditionalDetails().getUnlocationData().get("paidPlace")) : "");
                itemRow.createCell(headerMap.get("Issued Place Name")).setCellValue(shipment.getAdditionalDetails() != null && shipment.getAdditionalDetails().getUnlocationData() != null ? String.valueOf(shipment.getAdditionalDetails().getUnlocationData().get("placeOfIssue")) : "");
                itemRow.createCell(headerMap.get("Source1")).setCellValue(String.valueOf(shipment.getSource()));
                itemRow.createCell(headerMap.get("Date of Issue")).setCellValue(Objects.isNull(shipment.getAdditionalDetails()) || Objects.isNull(shipment.getAdditionalDetails().getDateOfIssue()) ? "" : shipment.getAdditionalDetails().getDateOfIssue().toString());
                itemRow.createCell(headerMap.get("Date of Receipt")).setCellValue(Objects.isNull(shipment.getAdditionalDetails()) || Objects.isNull(shipment.getAdditionalDetails().getDateOfReceipt()) ? "" : shipment.getAdditionalDetails().getDateOfReceipt().toString());
                itemRow.createCell(headerMap.get("Country of Origin")).setCellValue(Objects.isNull(shipment.getAdditionalDetails()) ? "" : shipment.getAdditionalDetails().getGoodsCO());
                itemRow.createCell(headerMap.get("Notify Party Name")).setCellValue(shipment.getAdditionalDetails() != null && shipment.getAdditionalDetails().getNotifyParty() != null && shipment.getAdditionalDetails().getNotifyParty().getOrgData() != null ?
                        String.valueOf(shipment.getAdditionalDetails().getNotifyParty().getOrgData().get("FullName")) : "");
                itemRow.createCell(headerMap.get("Cargo Type")).setCellValue(shipment.getShipmentType());
                itemRow.createCell(headerMap.get("Origin")).setCellValue(origin);
                itemRow.createCell(headerMap.get("Destination")).setCellValue(destination);
                itemRow.createCell(headerMap.get("Domestic")).setCellValue(String.valueOf(shipment.getIsDomestic()));
                itemRow.createCell(headerMap.get("Route")).setCellValue(shipment.getRoute());
                itemRow.createCell(headerMap.get("Client Name")).setCellValue(shipment.getClient() != null && shipment.getClient().getOrgData() != null ? shipment.getClient().getOrgData().getOrDefault(PartiesConstants.FULLNAME, "").toString() : "");
                itemRow.createCell(headerMap.get("Consignor Name")).setCellValue(shipment.getConsigner() != null && shipment.getConsigner().getOrgData() != null ? shipment.getConsigner().getOrgData().getOrDefault(PartiesConstants.FULLNAME, "").toString() : "");
                itemRow.createCell(headerMap.get("Consignee Name")).setCellValue(shipment.getConsignee() != null && shipment.getConsignee().getOrgData() != null ? shipment.getConsignee().getOrgData().getOrDefault(PartiesConstants.FULLNAME, "").toString() : "");
                itemRow.createCell(headerMap.get("HBL Number")).setCellValue(shipment.getHouseBill());
                itemRow.createCell(headerMap.get("BOE Number")).setCellValue(shipment.getAdditionalDetails() != null ? shipment.getAdditionalDetails().getBOENumber() : "");
                itemRow.createCell(headerMap.get("Screening Status")).setCellValue(shipment.getAdditionalDetails() != null ? shipment.getAdditionalDetails().getScreeningStatus() : "");
                itemRow.createCell(headerMap.get("BOE Date")).setCellValue(shipment.getAdditionalDetails() != null && shipment.getAdditionalDetails().getBOEDate() != null ? shipment.getAdditionalDetails().getBOEDate().toString() : "");
                itemRow.createCell(headerMap.get("ETD")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getEtd() != null ? shipment.getCarrierDetails().getEtd().toString() : "");
                itemRow.createCell(headerMap.get("ETA")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getEta() != null ? shipment.getCarrierDetails().getEta().toString() : "");
                itemRow.createCell(headerMap.get("ATD")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getAtd() != null ? shipment.getCarrierDetails().getAtd().toString() : "");
                itemRow.createCell(headerMap.get("ATA")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getAta() != null ? shipment.getCarrierDetails().getAta().toString() : "");
                itemRow.createCell(headerMap.get("Estimated Delivery")).setCellValue(shipment.getDeliveryDetails() != null && shipment.getDeliveryDetails().getEstimatedPickupOrDelivery() != null ? shipment.getDeliveryDetails().getEstimatedPickupOrDelivery().toString() : "");
                itemRow.createCell(headerMap.get("Actual Delivery")).setCellValue(shipment.getDeliveryDetails() != null && shipment.getDeliveryDetails().getActualPickupOrDelivery() != null ? shipment.getDeliveryDetails().getActualPickupOrDelivery().toString() : "");
                itemRow.createCell(headerMap.get("Goods Description")).setCellValue(shipment.getGoodsDescription());
                itemRow.createCell(headerMap.get("Gross Weight")).setCellValue(String.valueOf(shipment.getWeight()));
                itemRow.createCell(headerMap.get("Gross Weight Unit")).setCellValue(shipment.getWeightUnit());
                itemRow.createCell(headerMap.get("Volume")).setCellValue(String.valueOf(shipment.getVolume()));
                itemRow.createCell(headerMap.get("Volume Unit")).setCellValue(shipment.getVolumeUnit());
                itemRow.createCell(headerMap.get("Chargeable Weight")).setCellValue(String.valueOf(shipment.getChargable()));
                itemRow.createCell(headerMap.get("Volumetric Weight")).setCellValue(String.valueOf(shipment.getVolumetricWeight()));
                itemRow.createCell(headerMap.get("No. Of Packages")).setCellValue(String.valueOf(shipment.getNoOfPacks()));
                itemRow.createCell(headerMap.get("Package Type")).setCellValue("");
                itemRow.createCell(headerMap.get("No. Of Inner Packages")).setCellValue(String.valueOf(shipment.getInnerPacks()));
                itemRow.createCell(headerMap.get("IU")).setCellValue("");
                itemRow.createCell(headerMap.get("Customer Booking Number")).setCellValue(String.valueOf(shipment.getBookingNumber()));
                itemRow.createCell(headerMap.get("Pickup Transporter")).setCellValue(shipment.getPickupDetails() != null && shipment.getPickupDetails().getTransporterDetail() != null && shipment.getPickupDetails().getTransporterDetail().getOrgData() != null ? String.valueOf(shipment.getPickupDetails().getTransporterDetail().getOrgData().get("FullName")) : "");
                itemRow.createCell(headerMap.get("Delivery Transporter")).setCellValue(shipment.getDeliveryDetails() != null && shipment.getDeliveryDetails().getTransporterDetail() != null && shipment.getDeliveryDetails().getTransporterDetail().getOrgData() != null ? String.valueOf(shipment.getDeliveryDetails().getTransporterDetail().getOrgData().get("FullName")) : "");
                itemRow.createCell(headerMap.get("Job Status")).setCellValue(String.valueOf(shipment.getJobStatus()));
                itemRow.createCell(headerMap.get("Assigned To")).setCellValue(String.valueOf(shipment.getAssignedTo()));
                itemRow.createCell(headerMap.get("Created By")).setCellValue(String.valueOf(shipment.getCreatedBy()));
                itemRow.createCell(headerMap.get("Created Source")).setCellValue(String.valueOf(shipment.getSource()));
                itemRow.createCell(headerMap.get("Updated Date")).setCellValue(String.valueOf(shipment.getUpdatedAt()));
                itemRow.createCell(headerMap.get("20RE")).setCellValue(String.valueOf(shipment.getContainer20RECount()));
                itemRow.createCell(headerMap.get("20GP")).setCellValue(String.valueOf(shipment.getContainer20GPCount()));
                itemRow.createCell(headerMap.get("40RE")).setCellValue(String.valueOf(shipment.getContainer40RECount()));
                itemRow.createCell(headerMap.get("40GP")).setCellValue(String.valueOf(shipment.getContainer40GPCount()));
                itemRow.createCell(headerMap.get("Container Number")).setCellValue(shipment.getContainerNumbers() != null && !shipment.getContainerNumbers().isEmpty() ? shipment.getContainerNumbers().stream().findFirst().get() : "");
                itemRow.createCell(headerMap.get("Created Date")).setCellValue(String.valueOf(shipment.getCreatedAt()));
                itemRow.createCell(headerMap.get("Estimated Cost")).setCellValue(shipment.getTotalEstimatedCost() != null ? shipment.getTotalEstimatedCost().toString() : "");
                itemRow.createCell(headerMap.get("Estimated Revenue")).setCellValue(shipment.getTotalEstimatedRevenue() != null ? shipment.getTotalEstimatedRevenue().toString() : "");
                itemRow.createCell(headerMap.get("Estimated Profit")).setCellValue(shipment.getTotalEstimatedProfit() != null ? shipment.getTotalEstimatedProfit().toString() : "");
                itemRow.createCell(headerMap.get("Estimated Profit %")).setCellValue(shipment.getTotalEstimatedProfitPercent() != null ? shipment.getTotalEstimatedProfitPercent().toString() : "");
                itemRow.createCell(headerMap.get("Captured Cost")).setCellValue(shipment.getTotalCost() != null ? shipment.getTotalCost().toString() : "");
                itemRow.createCell(headerMap.get("Captured Revenue")).setCellValue(shipment.getTotalRevenue() != null ? shipment.getTotalRevenue().toString() : "");
                itemRow.createCell(headerMap.get("Captured Profit")).setCellValue(shipment.getTotalProfit() != null ? shipment.getTotalProfit().toString() : "");
                itemRow.createCell(headerMap.get("Captured Profit %")).setCellValue(shipment.getTotalProfitPercent() != null ? shipment.getTotalProfitPercent().toString() : "");
                itemRow.createCell(headerMap.get("Invoiced Payable Cost")).setCellValue(shipment.getTotalPostedProfit() != null ? shipment.getTotalPostedCost().toString() : "");
                itemRow.createCell(headerMap.get("Invoiced Receivable Revenue")).setCellValue(shipment.getTotalPostedRevenue() != null ? shipment.getTotalPostedRevenue().toString() : "");
                itemRow.createCell(headerMap.get("Invoiced Profit")).setCellValue(shipment.getTotalPostedProfit() != null ? shipment.getTotalPostedProfit().toString() : "");
                itemRow.createCell(headerMap.get("Invoiced Profit %")).setCellValue(shipment.getTotalPostedProfitPercent() != null ? shipment.getTotalPostedProfitPercent().toString() : "");
                itemRow.createCell(headerMap.get("20s Count")).setCellValue(String.valueOf(shipment.getContainer20Count()));
                itemRow.createCell(headerMap.get("40s Count")).setCellValue(String.valueOf(shipment.getContainer40Count()));
                itemRow.createCell(headerMap.get("TEU Count")).setCellValue(shipment.getTeuCount() != null ? shipment.getTeuCount().toString() : null);
                itemRow.createCell(headerMap.get("CreatedBy")).setCellValue(shipment.getCreatedBy());
                itemRow.createCell(headerMap.get("POL")).setCellValue(originPort);
                itemRow.createCell(headerMap.get("POD")).setCellValue(destinationPort);
                itemRow.createCell(headerMap.get("Waybill Number")).setCellValue(String.valueOf(shipment.getWayBillNumber()));
                itemRow.createCell(headerMap.get("Additional Terms")).setCellValue(String.valueOf(shipment.getAdditionalTerms()));
                itemRow.createCell(headerMap.get("Reference Number")).setCellValue(String.valueOf(shipment.getBookingReference()));
                itemRow.createCell(headerMap.get("POL Code")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getUnlocationData() != null ? String.valueOf(shipment.getCarrierDetails().getUnlocationData().get("originPort_code")) : "");
                itemRow.createCell(headerMap.get("POD Code")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getUnlocationData() != null ? String.valueOf(shipment.getCarrierDetails().getUnlocationData().get("destinationPort_code")) : "");
                itemRow.createCell(headerMap.get("Origin Code")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getUnlocationData() != null ? String.valueOf(shipment.getCarrierDetails().getUnlocationData().get("origin_code")) : "");
                itemRow.createCell(headerMap.get("Destination Code")).setCellValue(shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getUnlocationData() != null ? String.valueOf(shipment.getCarrierDetails().getUnlocationData().get("destination_code")) : "");
            }

            LocalDateTime currentTime = LocalDateTime.now();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_HH_MM_SS_FORMAT);
            String timestamp = currentTime.format(formatter);
            String filenameWithTimestamp = "Shipments_" + timestamp + Constants.XLSX;

            response.setContentType(Constants.CONTENT_TYPE_FOR_EXCEL);
            response.setHeader("Content-Disposition", "attachment; filename=" + filenameWithTimestamp);

            try (OutputStream outputStream = new BufferedOutputStream(response.getOutputStream(), 8192 * 10)) {
                workbook.write(outputStream);
            } catch (IOException e) {
                log.error("Time out " + e.getMessage());
            }
        }

    }

    private void makeHeadersInSheet(Sheet sheet, Workbook workbook) {
        Row headerRow = sheet.createRow(0);
        List<String> shipmentHeader = ShipmentConstants.SHIPMENT_HEADERS;

        CellStyle boldStyle = workbook.createCellStyle();
        Font boldFont = workbook.createFont();
        boldFont.setBold(true);
        boldStyle.setFont(boldFont);

        for (int i = 0; i < shipmentHeader.size(); i++) {
            Cell cell = headerRow.createCell(i);
            cell.setCellValue(shipmentHeader.get(i));
            cell.setCellStyle(boldStyle);
        }
    }


    public ResponseEntity<IRunnerResponse> fullShipmentsList(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
            }
            request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS));
//            checkWayBillNumberCriteria(request);
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info(ShipmentConstants.SHIPMENT_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildListSuccessResponse(
                        convertEntityListToFullShipmentList(shipmentDetailsPage.getContent()),
                        shipmentDetailsPage.getTotalPages(),
                        shipmentDetailsPage.getTotalElements());
            else {
                List<IRunnerResponse>filteredList=new ArrayList<>();
                for( var curr: convertEntityListToFullShipmentList(shipmentDetailsPage.getContent())){
                    RunnerPartialListResponse res=new RunnerPartialListResponse();
                    res.setData(PartialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
                    filteredList.add( res);

                }
                return ResponseHelper.buildListSuccessResponse(
                        filteredList,
                        shipmentDetailsPage.getTotalPages(),
                        shipmentDetailsPage.getTotalElements());
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> assignShipmentContainers(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ShipmentContainerAssignRequest request = (ShipmentContainerAssignRequest) commonRequestModel.getData();
            ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
            ShipmentDetails shipmentDetails = shipmentDao.findById(request.getShipmentId()).get();
            if(shipmentSettingsDetails.getMultipleShipmentEnabled() && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA))) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("id", request.getContainerIds(), "IN");
                Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
                Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
                boolean isFCL = shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA));
                if(containers != null && containers.getContent() != null) {
                    List<Containers> containersList = containers.getContent();
                    if(!containers.getContent().isEmpty()) {
                        for (Containers container : containersList) {
                            boolean isPart = container.getIsPart() != null && container.getIsPart().booleanValue();
                            if((shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) || !isPart) && container.getShipmentsList() != null && container.getShipmentsList().size() > 0) {
                                String errorMsg = "This container is already linked to another shipment. Only part Container/Containers are allowed to attach";
                                if(isPart)
                                    errorMsg = "Mentioned container " + container.getContainerNumber() + " is already assigned to a Shipment - " + container.getShipmentsList().get(0).getShipmentId() + ". Please check and retry.";
                                throw new ValidationException(errorMsg);
                            }
                            if(isFCL) {
                                container.setAchievedWeight(container.getAllocatedWeight());
                                container.setAchievedVolume(container.getAllocatedVolume());
                                container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
                                container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
                                container.setWeightUtilization("100");
                                container.setVolumeUtilization("100");
                            }
                        }
                    }
                    if(isFCL)
                        containerDao.saveAll(containersList);
                }
            }
            shipmentsContainersMappingDao.assignContainers(request.getShipmentId(), request.getContainerIds());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> assignAllContainers(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        boolean lclAndSeaOrRoadFlag = shipmentSettingsDetails.getMultipleShipmentEnabled() != null && shipmentSettingsDetails.getMultipleShipmentEnabled();
        boolean IsConsolidatorFlag = shipmentSettingsDetails.getIsConsolidator() != null && shipmentSettingsDetails.getIsConsolidator();
        List<Containers> containersList = new ArrayList<>();
        try {
            ContainerAssignListRequest containerAssignRequest = (ContainerAssignListRequest) commonRequestModel.getData();
            Long shipmentId = containerAssignRequest.getShipmentId();
            Long consolidationId = containerAssignRequest.getConsolidationId();
            if (lclAndSeaOrRoadFlag) {
                if(!containerAssignRequest.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) && !containerAssignRequest.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA)) {
                    lclAndSeaOrRoadFlag = false;
                }
            }
            ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", consolidationId, "=");
            Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
            Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
            List<Containers> conts = new ArrayList<>();
            List<Long> containerIds = new ArrayList<>();
            if(lclAndSeaOrRoadFlag) {
                for (Containers container : containers.getContent()) {
                    List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(container.getId());
                    if(!shipmentsContainersMappings.stream().map(ShipmentsContainersMapping::getShipmentId).toList().contains(shipmentId)) {

                        if(container.getAllocatedWeight() != null && container.getAchievedWeight() != null && container.getAllocatedVolume() != null && container.getAchievedWeight() != null
                                && isNotEmpty(container.getAllocatedWeightUnit()) && isNotEmpty(container.getAllocatedVolumeUnit()) && isNotEmpty(container.getAchievedWeightUnit()) && isNotEmpty(container.getAchievedVolumeUnit())) {

                            BigDecimal achievedWeight = new BigDecimal(convertUnit(Constants.MASS, container.getAchievedWeight(), container.getAchievedWeightUnit(), container.getAllocatedWeightUnit()).toString());
                            BigDecimal achievedVolume = new BigDecimal(convertUnit(Constants.VOLUME, container.getAchievedVolume(), container.getAchievedVolumeUnit(), container.getAllocatedVolumeUnit()).toString());

                            if(achievedWeight.compareTo(container.getAllocatedWeight()) < 0 && achievedVolume.compareTo(container.getAllocatedVolume()) < 0) {
                                containersList.add(container);
                            }
                            else if(!IsConsolidatorFlag) {
                                conts.add(container);
                            }
                        }
                        else
                            containersList.add(container);
                    }
                }
                if(conts.size() > 0) {
                    for (Containers x : conts) {
                        boolean flag = true;
                        if(x.getShipmentsList() != null && x.getShipmentsList().size() > 0) {
                            for(ShipmentDetails shipmentDetails : x.getShipmentsList()) {
                                if(shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL))
                                    flag = false;
                            }
                        }
                        if (flag)
                            containersList.add(x);
                    }
                }

                ShipmentDetails shipmentDetails = shipmentDao.findById(containerAssignRequest.getShipmentId()).get();
                boolean isFCL = shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA));
                for (Containers container : containersList) {
                    boolean isPart = container.getIsPart() != null && container.getIsPart().booleanValue();
                    if ((shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) || isPart) && container.getShipmentsList() != null && container.getShipmentsList().size() > 0) {

                    }
                    else {
                        containerIds.add(container.getId());
                    }
                    if (isFCL) {
                        container.setAchievedWeight(container.getAllocatedWeight());
                        container.setAchievedVolume(container.getAllocatedVolume());
                        container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
                        container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
                        container.setWeightUtilization("100");
                        container.setVolumeUtilization("100");
                    }
                }
                if(isFCL)
                    containerDao.saveAll(containersList);
            }
            else {
                for (Containers container : containers.getContent()) {
                    List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(container.getId());
                    if(shipmentsContainersMappings.isEmpty()) {
                        containerIds.add(container.getId());
                    }
                }
            }
            if(!Objects.isNull(containerIds) && !containerIds.isEmpty())
                shipmentsContainersMappingDao.assignContainers(containerAssignRequest.getShipmentId(), containerIds);
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private List<IRunnerResponse> convertEntityListToFullShipmentList(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(shipmentDetail -> {
            ShipmentDetailsResponse response = modelMapper.map(shipmentDetail, ShipmentDetailsResponse.class);
            // TODO- check if they want status
//            if (shipmentDetail.getStatus() != null && shipmentDetail.getStatus() < ShipmentStatus.values().length)
//                response.setShipmentStatus(ShipmentStatus.values()[shipmentDetail.getStatus()].toString());
            responseList.add(response);
        });
//        setLocationData(responseList);
//        setContainerTeu(lst, responseList);
//        setBillingData(lst, responseList);
        return responseList;
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_LIST_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
            }
            request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS));
            checkWayBillNumberCriteria(request);
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info(ShipmentConstants.SHIPMENT_LIST_RESPONSE_SUCCESS, LoggerHelper.getRequestIdFromMDC());
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildListSuccessResponse(
                        convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                        shipmentDetailsPage.getTotalPages(),
                        shipmentDetailsPage.getTotalElements());
            else {
                List<IRunnerResponse>filtered_list=new ArrayList<>();
                for( var curr: convertEntityListToDtoList(shipmentDetailsPage.getContent())){
                    RunnerPartialListResponse res=new RunnerPartialListResponse();
                    res.setData(PartialFetchUtils.fetchPartialListData(curr,request.getIncludeColumns()));
                    filtered_list.add( res);

                }
                return ResponseHelper.buildListSuccessResponse(
                        filtered_list,
                        shipmentDetailsPage.getTotalPages(),
                        shipmentDetailsPage.getTotalElements());
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    private void checkWayBillNumberCriteria(ListCommonRequest request)
    {
        if(request != null && request.getFilterCriteria() != null && request.getFilterCriteria().size() > 0)
        {
            checkForWayBillFilter(request.getFilterCriteria());
        }
    }

    private void checkForWayBillFilter(List<FilterCriteria> filterCriteriaList) {
        for(FilterCriteria filterCriteria: filterCriteriaList)
        {
            if(filterCriteria.getCriteria() != null && filterCriteria.getCriteria().getFieldName() != null &&
                    filterCriteria.getCriteria().getFieldName().equals("wayBillNumber") && filterCriteria.getCriteria().getValue() != null) {

                WayBillNumberFilterRequest wayBillNumberFilterRequest = new WayBillNumberFilterRequest();
                wayBillNumberFilterRequest.setWayBillNumber(filterCriteria.getCriteria().getValue().toString());
                GuidsListResponse guidsListResponse = v1Service.fetchWayBillNumberFilterGuids(wayBillNumberFilterRequest);
                filterCriteria.getCriteria().setFieldName("guid");
                filterCriteria.getCriteria().setOperator("IN");
                filterCriteria.getCriteria().setValue(guidsListResponse.getGuidsList());
            }
            if(filterCriteria.getInnerFilter() != null && filterCriteria.getInnerFilter().size() > 0) {
                checkForWayBillFilter(filterCriteria.getInnerFilter());
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
                log.error("Request is empty for Shipment async list for Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException(ShipmentConstants.SHIPMENT_LIST_REQUEST_NULL_ERROR);
            }
            request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS));
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Shipment async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                    shipmentDetailsPage.getTotalPages(),
                    shipmentDetailsPage.getTotalElements()));
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
                log.debug("Request is empty for Shipment delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for Shipment delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            String oldEntityJsonString = jsonHelper.convertToJson(shipmentDetails.get());
            shipmentDao.delete(shipmentDetails.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, ShipmentDetails.class))
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(shipmentDetails.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );

            log.info("Deleted Shipment details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
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
            double start = System.currentTimeMillis();
            if(request.getId() == null && request.getGuid() == null) {
                log.error("Request Id and Guid are null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException("Id and GUID can't be null. Please provide any one !");
            }
            Long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = Optional.ofNullable(null);
            if(id != null ){
                shipmentDetails = shipmentDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                shipmentDetails = shipmentDao.findByGuid(guid);
            }
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for the input with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            List<Notes> notes = notesDao.findByEntityIdAndEntityType(request.getId(), Constants.CUSTOMER_BOOKING);
            double current = System.currentTimeMillis();
            log.info("Shipment details fetched successfully for Id {} with Request Id {} within: {}ms", id, LoggerHelper.getRequestIdFromMDC(), current - start);
            ShipmentDetailsResponse response = modelMapper.map(shipmentDetails.get(), ShipmentDetailsResponse.class);
            log.info("Request: {} || Time taken for model mapper: {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - current);
            response.setCustomerBookingNotesList(jsonHelper.convertValueToList(notes,NotesResponse.class));
            createShipmentPayload(shipmentDetails.get(), response);
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
                log.error("Request is empty for Shipment async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null && request.getGuid() == null) {
                log.error("Request Id is null for Shipment async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = Optional.ofNullable(null);
            if(request.getId() != null ){
                shipmentDetails = shipmentDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                shipmentDetails = shipmentDao.findByGuid(guid);
            }
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for the input with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if (!shipmentDetails.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            shipmentDetails.get().setNotesList(notesDao.findByEntityIdAndEntityType(id, Constants.SHIPMENT));
            log.info("Shipment details async fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ShipmentDetailsResponse response = jsonHelper.convertValue(shipmentDetails.get(), ShipmentDetailsResponse.class);
            response.setCustomerBookingNotesList(convertToDtoList(notesDao.findByEntityIdAndEntityType(request.getId(), Constants.CUSTOMER_BOOKING),NotesResponse.class));
            //containerCountUpdate(shipmentDetails.get(), response);
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(response));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    public ResponseEntity<IRunnerResponse> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException {
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request.getId() == null && request.getGuid() == null) {
                log.error("Request Id and Guid are null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException("Id and GUID can't be null. Please provide any one !");
            }
            CompletableFuture<ResponseEntity<IRunnerResponse>> shipmentsFuture = retrieveByIdAsync(commonRequestModel);
            RunnerResponse<ShipmentDetailsResponse> res = (RunnerResponse<ShipmentDetailsResponse>) shipmentsFuture.get().getBody();
            if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
                return ResponseHelper.buildSuccessResponse(res.getData());
            else
                return ResponseHelper.buildSuccessResponse(PartialFetchUtils.fetchPartialData(res, request.getIncludeColumns()));
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> partialUpdate(CommonRequestModel commonRequestModel, Boolean fromV1) throws RunnerException {

        ShipmentPatchRequest shipmentRequest = (ShipmentPatchRequest) commonRequestModel.getData();
        if ((shipmentRequest.getId() == null && shipmentRequest.getGuid() == null) && (shipmentRequest.getShipmentId() == null || shipmentRequest.getShipmentId().get() == "")) {
            log.error("Request Id is null for update request with Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Request Id is null");
        }
        List<BookingCarriageRequest> bookingCarriageRequestList = shipmentRequest.getBookingCarriagesList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = shipmentRequest.getTruckDriverDetails();
        List<PackingRequest> packingRequestList = shipmentRequest.getPackingList();
        AdditionalDetailRequest additionalDetailRequest = shipmentRequest.getAdditionalDetail();
        List<ContainerRequest> containerRequestList = shipmentRequest.getContainersList();
        List<ELDetailsRequest> elDetailsRequestList = shipmentRequest.getElDetailsList();
        List<EventsRequest> eventsRequestList = shipmentRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = shipmentRequest.getFileRepoList();
        List<JobRequest> jobRequestList = shipmentRequest.getJobsList();
        List<NotesRequest> notesRequestList = shipmentRequest.getNotesList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = shipmentRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = shipmentRequest.getRoutingsList();
        List<ServiceDetailsRequest> serviceDetailsRequestList = shipmentRequest.getServicesList();
        CarrierPatchRequest carrierDetailRequest = shipmentRequest.getCarrierDetails();
        // TODO- implement Validation logic
        Long id = null;
        Optional<ShipmentDetails> oldEntity;
        ShipmentRequest fetchShipmentRequest = new ShipmentRequest();
        fetchShipmentRequest.setId(shipmentRequest.getId() != null ? shipmentRequest.getId().get() : null);
        fetchShipmentRequest.setGuid(shipmentRequest.getGuid());
        if(shipmentRequest.getId() != null || shipmentRequest.getGuid() != null) {
            oldEntity = retrieveByIdOrGuid(fetchShipmentRequest);
            id = oldEntity.get().getId();
        }
        else {
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, shipmentRequest.getShipmentId().get(), "=");
            Pair<Specification<ShipmentDetails>, Pageable> shipmentPair = fetchData(listCommonRequest, ShipmentDetails.class);
            Page<ShipmentDetails> shipmentDetails = shipmentDao.findAll(shipmentPair.getLeft(), shipmentPair.getRight());
            if(shipmentDetails != null && shipmentDetails.get().count() == 1) {
                oldEntity = shipmentDetails.get().findFirst();
                id = oldEntity.get().getId();
            }
            else if(shipmentDetails == null || shipmentDetails.get().count() == 0) {
                log.error("Shipment not available for update request with Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            else {
                log.error("More than one shipments available for update request with Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INCORRECT_RESULT_SIZE_EXCEPTION_MSG);
            }
        }
        if (!oldEntity.isPresent()) {
            log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_ID_ERROR, shipmentRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try {
            ShipmentDetails entity = oldEntity.get();
            Integer previousStatus = oldEntity.get().getStatus();
            shipmentDetailsMapper.update(shipmentRequest, entity);
            ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
            entity.setId(oldEntity.get().getId());
            List<Containers> updatedContainers = null;
            Long consolidationId = null;
            if(entity.getConsolidationList() != null && entity.getConsolidationList().size() > 0)
                consolidationId = entity.getConsolidationList().get(0).getId();
            if (containerRequestList != null) {
                updatedContainers = containerDao.updateEntityFromShipmentConsole(convertToEntityList(containerRequestList, Containers.class), consolidationId, id, false);
            } else {
                updatedContainers = oldEntity.get().getContainersList();
            }
            entity.setContainersList(updatedContainers);
            AdditionalDetails updatedAdditionalDetails = null;
            if (additionalDetailRequest != null) {
                updatedAdditionalDetails = additionalDetailDao.updateEntityFromShipment(jsonHelper.convertValue(additionalDetailRequest, AdditionalDetails.class));
                entity.setAdditionalDetails(updatedAdditionalDetails);
            }
            CarrierDetails updatedCarrierDetails = null;
            if (carrierDetailRequest != null) {
                updatedCarrierDetails = oldEntity.get().getCarrierDetails();
                carrierDetailsMapper.update(carrierDetailRequest, updatedCarrierDetails);
                entity.setCarrierDetails(oldEntity.get().getCarrierDetails());
            }
            entity.setCarrierDetails(oldEntity.get().getCarrierDetails());
            validateBeforeSave(entity);

            ConsolidationDetails consolidationDetails = updateLinkedShipmentData(entity, oldEntity.get());
            if(!Objects.isNull(consolidationDetails)) {
                entity.setConsolidationList(new ArrayList<>(Arrays.asList(consolidationDetails)));
            }
            entity = shipmentDao.update(entity, false);

            entity.setContainersList(updatedContainers);
            if (additionalDetailRequest != null) {
                entity.setAdditionalDetails(updatedAdditionalDetails);
            }
            if (carrierDetailRequest != null) {
                entity.setCarrierDetails(updatedCarrierDetails);
            }
            if (bookingCarriageRequestList != null) {
                List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(jsonHelper.convertValueToList(bookingCarriageRequestList, BookingCarriage.class), id);
                entity.setBookingCarriagesList(updatedBookingCarriages);
            }
            if (truckDriverDetailsRequestList != null) {
                List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(truckDriverDetailsRequestList, TruckDriverDetails.class), id);
                entity.setTruckDriverDetails(updatedTruckDriverDetails);
            }
            if (packingRequestList != null) {
                List<Packing> updatedPackings = packingDao.updateEntityFromShipment(jsonHelper.convertValueToList(packingRequestList, Packing.class), id, null);
                entity.setPackingList(updatedPackings);
            }
            if (elDetailsRequestList != null) {
                List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(elDetailsRequestList, ELDetails.class), id);
                entity.setElDetailsList(updatedELDetails);
            }
            if (eventsRequestList != null) {
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(jsonHelper.convertValueToList(eventsRequestList, Events.class), id, Constants.SHIPMENT);
                entity.setEventsList(updatedEvents);
                eventService.updateAtaAtdInShipment(updatedEvents, entity, shipmentSettingsDetails);
            }
            // Create events on basis of shipment status Confirmed/Created
            autoGenerateEvents(entity, previousStatus);

            if (notesRequestList != null) {
                List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(jsonHelper.convertValueToList(notesRequestList, Notes.class), id, Constants.SHIPMENT);
                entity.setNotesList(updatedNotes);
            }
            if (referenceNumbersRequestList != null) {
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(jsonHelper.convertValueToList(referenceNumbersRequestList, ReferenceNumbers.class), id);
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (routingsRequestList != null) {
                List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(jsonHelper.convertValueToList(routingsRequestList, Routings.class), id);
                entity.setRoutingsList(updatedRoutings);
            }
            if (serviceDetailsRequestList != null) {
                List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(serviceDetailsRequestList, ServiceDetails.class), id);
                entity.setServicesList(updatedServiceDetails);
            }

            if(fromV1 == null || !fromV1) {
                syncShipment(entity, null, null, null, consolidationDetails, true);
            }

            pushShipmentDataToDependentService(entity, false);
            ShipmentDetailsResponse response = shipmentDetailsMapper.map(entity);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> toggleLock(CommonRequestModel commonRequestModel) throws RunnerException {
        CommonGetRequest commonGetRequest = (CommonGetRequest) commonRequestModel.getData();
        Long id = commonGetRequest.getId();
        ShipmentDetails shipmentDetails = shipmentDao.findById(id).get();
        String lockingUser = shipmentDetails.getLockedBy();
        String currentUser = userContext.getUser().getUsername();

        if (shipmentDetails.getIsLocked() != null && shipmentDetails.getIsLocked()) {
            if (lockingUser != null && (Objects.equals(lockingUser, currentUser) ||
                    (!Objects.isNull(PermissionsContext.getPermissions(PermissionConstants.tenantSuperAdmin)) && !PermissionsContext.getPermissions(PermissionConstants.tenantSuperAdmin).isEmpty()) ))
                shipmentDetails.setIsLocked(false);
            else
                throw new RunnerException(String.format(ErrorConstants.LOCK_UNLOCK_ERROR, Constants.Shipment, lockingUser));
        } else {
            shipmentDetails.setIsLocked(true);
            shipmentDetails.setLockedBy(currentUser);
        }
        shipmentDetails = shipmentDao.save(shipmentDetails, false);
        shipmentSync.syncLockStatus(shipmentDetails);
        pushShipmentDataToDependentService(shipmentDetails, false);
        return ResponseHelper.buildSuccessResponse();
    }


    private <T extends IRunnerResponse> List<T> getResponse(CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntity) throws ExecutionException, InterruptedException {
        RunnerListResponse runnerListResponse = (RunnerListResponse<T>) responseEntity.get().getBody();
        return (List<T>) runnerListResponse.getData();
    }

    private <T extends IRunnerResponse> List<T> getResponse(ResponseEntity<?> responseEntity) throws ExecutionException, InterruptedException {
        RunnerListResponse runnerListResponse = (RunnerListResponse<T>) responseEntity.getBody();
        return (List<T>) runnerListResponse.getData();
    }

    private <T extends IRunnerResponse> T getResponseEntity(ResponseEntity<?> responseEntity) throws ExecutionException, InterruptedException {
        RunnerResponse runnerResponse = (RunnerResponse<T>) responseEntity.getBody();
        return (T) runnerResponse.getData();
    }

    private String generateShipmentId(ShipmentDetails shipmentDetails) {
        List<ShipmentSettingsDetails> shipmentSettingsList = shipmentSettingsDao.list();
        String shipmentId = "";
        boolean flag = true;

        while(flag) {
            ListCommonRequest listRequest = constructListCommonRequest(Constants.SHIPMENT_ID, shipmentId, "=");
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listRequest, ShipmentDetails.class);
            Page<ShipmentDetails> shipments = shipmentDao.findAll(pair.getLeft(), pair.getRight());

            if(!shipmentId.isEmpty() && shipments.getTotalElements() == 0)
                flag = false;
            else {
                if(shipmentSettingsList != null && shipmentSettingsList.size() != 0 && shipmentSettingsList.get(0) != null && shipmentSettingsList.get(0).getCustomisedSequence()) {
                    try{
                        shipmentId = getCustomizedShipmentProcessNumber(shipmentSettingsList.get(0), ProductProcessTypes.ShipmentNumber, shipmentDetails);
                    } catch (Exception ignored) {
                        //
                        shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
                    }
                }
                if(StringUtility.isEmpty(shipmentId)) {
                    shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
                }
            }
        }
        return shipmentId;
//        if (shipmentSettingsList.isEmpty())
//            return StringUtility.getRandomString(10);
//        return createShipmentSequence(shipmentSettingsList.get(0));
    }

    private String getCustomizedShipmentProcessNumber(ShipmentSettingsDetails shipmentSettingsDetails, ProductProcessTypes productProcessType, ShipmentDetails currentShipment) throws RunnerException {
        List<TenantProducts> tenantProducts = productEngine.populateEnabledTenantProducts(shipmentSettingsDetails);
        // to check the commmon sequence
        var sequenceNumber = productEngine.GetCommonSequenceNumber(currentShipment.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI);
        if (sequenceNumber != null && !sequenceNumber.isEmpty()) {
            return sequenceNumber;
        }
        var identifiedProduct = productEngine.IdentifyProduct(currentShipment, tenantProducts);
        if (identifiedProduct == null){
            return "";
        }
        var sequenceSettings = getNextNumberHelper.getProductSequence(identifiedProduct.getId(), productProcessType);
        if(sequenceSettings == null){
            sequenceSettings = productEngine.getShipmentProductWithOutContainerType(currentShipment, productProcessType, tenantProducts);
            if (sequenceSettings == null)
            {
                // get default product type for shipment
                var defaultProduct = productEngine.getDefaultShipmentProduct(tenantProducts);
                if (defaultProduct == null || identifiedProduct == defaultProduct) {
                    return "";
                }
                sequenceSettings = getNextNumberHelper.getProductSequence(defaultProduct.getId(), productProcessType);
                if (sequenceSettings == null) {
                    return "";
                }
            }
        }
        String prefix = sequenceSettings.getPrefix() == null ? "" : sequenceSettings.getPrefix();
        var user = UserContext.getUser();
        return getNextNumberHelper.generateCustomSequence(sequenceSettings, prefix, user.TenantId, true, null, false);
    }

    private String getShipmentsSerialNumber() {
        // Moving this responsibility to v1 sequnce table to avoid syncing overhead
        return v1Service.getShipmentSerialNumber();
    }

    private String createShipmentSequence(ShipmentSettingsDetails shipmentSetting) {
        String sequence = generateSequence(shipmentSetting.getShipmentIdGenerationType(), shipmentSetting.getShipmentIdGenerationPrefix(), shipmentSetting.getShipmentIdGenerationCounter());
        if (shipmentSetting.getShipmentIdGenerationType() == GenerationType.Serial) {
            shipmentSetting.setShipmentIdGenerationCounter(shipmentSetting.getShipmentIdGenerationCounter() + 1);
            shipmentSettingsDao.save(shipmentSetting);
        }
        return sequence;
    }

    private String generateSequence(GenerationType generationType, String prefix, Integer counter) {
        if(generationType == null)
            return StringUtility.getRandomString(10);
        String suffix;
        switch (generationType) {
            case Random:
                suffix = StringUtility.getRandomString(10);
                break;
            case Serial:
                suffix = String.valueOf(counter);
                break;
            default:
                suffix = StringUtility.getEmptyString();
        }
        return !StringUtils.isEmpty(prefix) ? prefix + suffix : suffix;
    }

    public ResponseEntity<IRunnerResponse> syncShipmentAuditLogsToService(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            AuditLogsSyncRequest request = (AuditLogsSyncRequest) commonRequestModel.getData();
            if(request.getGuid() == null)
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            Optional<ShipmentDetails> oldEntity = shipmentDao.findByGuid(request.getGuid());
            if(oldEntity.isPresent())
                syncEntityConversionService.auditLogsV1ToV2(request.getChangeLogs(), oldEntity.get().getId());
            else
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e){
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> completeV1ShipmentCreateAndUpdate(CommonRequestModel commonRequestModel, Map<UUID, String> map, List<NotesRequest> customerBookingNotes, boolean dataMigration, List<AuditLogRequestV2> auditLogRequestV2, String createdBy) throws RunnerException {

        ShipmentRequest shipmentRequest = (ShipmentRequest) commonRequestModel.getData();

        List<BookingCarriageRequest> bookingCarriageRequestList = shipmentRequest.getBookingCarriagesList();
        List<TruckDriverDetailsRequest> truckDriverDetailsRequestList = shipmentRequest.getTruckDriverDetails();
        List<PackingRequest> packingRequestList = shipmentRequest.getPackingList();
        AdditionalDetailRequest additionalDetailRequest = shipmentRequest.getAdditionalDetails();
        List<ContainerRequest> containerRequestList = shipmentRequest.getContainersList();
        List<ELDetailsRequest> elDetailsRequestList = shipmentRequest.getElDetailsList();
        List<EventsRequest> eventsRequestList = shipmentRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = shipmentRequest.getFileRepoList();
        List<JobRequest> jobRequestList = shipmentRequest.getJobsList();
        List<NotesRequest> notesRequestList = shipmentRequest.getNotesList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = shipmentRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = shipmentRequest.getRoutingsList();
        List<ServiceDetailsRequest> serviceDetailsRequestList = shipmentRequest.getServicesList();
        List<PartiesRequest> shipmentAddressesRequestList = shipmentRequest.getShipmentAddresses();
        CarrierDetailRequest carrierDetailRequest = shipmentRequest.getCarrierDetails();

        // TODO- implement Validation logic
        UUID guid = shipmentRequest.getGuid();
        Optional<ShipmentDetails> oldEntity = shipmentDao.findByGuid(guid);

        if (dataMigration) {
            MDC.put("skip-audit-log", "true");
        }

        List<ConsolidationDetails> tempConsolidations = new ArrayList<>();

        List<ConsolidationDetailsRequest> consolidationDetailsRequests = shipmentRequest.getConsolidationList();
        if(consolidationDetailsRequests != null && !consolidationDetailsRequests.isEmpty()) {
            for(ConsolidationDetailsRequest consolidation : consolidationDetailsRequests) {
                Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findByGuid(consolidation.getGuid());
                if(consolidationDetails.isPresent()) {
                    tempConsolidations.add(consolidationDetails.get());
                }
            }
        }

        try {
            List<Containers> oldContainers = null;
            Long id = null;
            ShipmentDetails oldShipment = null;
            boolean isCreate = true;
            List<Containers> containers;
            if(oldEntity.isPresent()) {
                oldShipment = oldEntity.get();
                id = oldEntity.get().getId();
                containers = oldEntity.get().getContainersList();
                if(containers != null && !containers.isEmpty()) {
                    if(oldContainers == null)
                        oldContainers = new ArrayList<>();
                    oldContainers.addAll(containers);
                }
                isCreate = false;
            }
            shipmentRequest.setConsolidationList(null);
            ShipmentDetails entity = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
            if (!tempConsolidations.isEmpty())
                entity.setConsolidationList(tempConsolidations);
            entity.setId(id);
            List<Containers> updatedContainers = null;
            if (containerRequestList != null) {
                containerRequestList.forEach(e -> e.setShipmentsList(null));
                if(!tempConsolidations.isEmpty() && !entity.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                    ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, tempConsolidations.get(0).getId(), "=");
                    Pair<Specification<Containers>, Pageable> containerPair = fetchData(listCommonRequest, Containers.class);
                    Page<Containers> oldConsolContainers = containerDao.findAll(containerPair.getLeft(), containerPair.getRight());
                    if(!oldConsolContainers.isEmpty()) {
                        if(oldContainers == null)
                            oldContainers = new ArrayList<>();
                        oldContainers.addAll(oldConsolContainers.getContent());
                    }
                }
                updatedContainers = containerDao.updateEntityFromShipmentV1(jsonHelper.convertValueToList(containerRequestList, Containers.class), oldContainers);
            } else if(!oldEntity.isEmpty()){
                updatedContainers = oldEntity.get().getContainersList();
            }
            entity.setContainersList(updatedContainers);
            String operation = DBOperationType.CREATE.name();
            String oldEntityJsonString = null;
            if(id == null) {
                entity = shipmentDao.save(entity, true);
                id = entity.getId();
            } else {
                operation = DBOperationType.UPDATE.name();
                oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
                entity = shipmentDao.update(entity, true);
            }

            shipmentDao.saveCreatedDateAndUser(id, createdBy, entity.getShipmentCreatedOn());

            createAuditLog(entity, oldEntityJsonString, operation);
            if (dataMigration) {
                createV1AuditLogs(entity.getId(), auditLogRequestV2);
            }
//            Not needed, added consolidations while saving shipment
//            attachConsolidations(entity.getId(), tempConsolIds);

            if (bookingCarriageRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
                Pair<Specification<BookingCarriage>, Pageable> bookingCarriagePair = fetchData(listCommonRequest, BookingCarriage.class);
                Page<BookingCarriage> oldBookingCarriages = bookingCarriageDao.findAll(bookingCarriagePair.getLeft(), bookingCarriagePair.getRight());
                List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(jsonHelper.convertValueToList(bookingCarriageRequestList, BookingCarriage.class), id, oldBookingCarriages.stream().toList());
                entity.setBookingCarriagesList(updatedBookingCarriages);
            }
            if (truckDriverDetailsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
                Pair<Specification<TruckDriverDetails>, Pageable> truckDriverDetailsPair = fetchData(listCommonRequest, TruckDriverDetails.class);
                Page<TruckDriverDetails> oldTruckDriverDetails = truckDriverDetailsDao.findAll(truckDriverDetailsPair.getLeft(), truckDriverDetailsPair.getRight());
                List<TruckDriverDetails> updatedTruckDriverDetails = truckDriverDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(truckDriverDetailsRequestList, TruckDriverDetails.class), id, oldTruckDriverDetails.stream().toList());
                entity.setTruckDriverDetails(updatedTruckDriverDetails);
            }
            if (packingRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
                Pair<Specification<Packing>, Pageable> packingPair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> oldPackings = packingDao.findAll(packingPair.getLeft(), packingPair.getRight());
                List<Packing> oldConsolPackings = new ArrayList<>();
                if(entity.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && !tempConsolidations.isEmpty()) {
                    listCommonRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, tempConsolidations.get(0).getId(), "=");
                    packingPair = fetchData(listCommonRequest, Packing.class);
                    oldConsolPackings = packingDao.findAll(packingPair.getLeft(), packingPair.getRight()).stream().toList();
                }
                List<Packing> updatedPackings = packingDao.updateEntityFromShipment(jsonHelper.convertValueToList(packingRequestList, Packing.class), id, oldPackings.stream().toList(), oldConsolPackings, updatedContainers, map);
                entity.setPackingList(updatedPackings);
            }
            if (elDetailsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
                Pair<Specification<ELDetails>, Pageable> elDetailsPair = fetchData(listCommonRequest, ELDetails.class);
                Page<ELDetails> oldELDetails = elDetailsDao.findAll(elDetailsPair.getLeft(), elDetailsPair.getRight());
                List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(elDetailsRequestList, ELDetails.class), id, oldELDetails.stream().toList());
                entity.setElDetailsList(updatedELDetails);
            }
            if (eventsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT);
                Pair<Specification<Events>, Pageable> pair = fetchData(listCommonRequest, Events.class);
                Page<Events> oldEvents = eventDao.findAll(pair.getLeft(), pair.getRight());
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(jsonHelper.convertValueToList(eventsRequestList, Events.class), id, Constants.SHIPMENT, oldEvents.stream().toList());
                entity.setEventsList(updatedEvents);
            }
            if (referenceNumbersRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
                Pair<Specification<ReferenceNumbers>, Pageable> pair = fetchData(listCommonRequest, ReferenceNumbers.class);
                Page<ReferenceNumbers> oldReferenceNumbers = referenceNumbersDao.findAll(pair.getLeft(), pair.getRight());
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(jsonHelper.convertValueToList(referenceNumbersRequestList, ReferenceNumbers.class), id, oldReferenceNumbers.stream().toList());
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (routingsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
                Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
                Page<Routings> oldRoutings = routingsDao.findAll(pair.getLeft(), pair.getRight());
                List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(jsonHelper.convertValueToList(routingsRequestList, Routings.class), id, oldRoutings.stream().toList());
                entity.setRoutingsList(updatedRoutings);
            }
            if (serviceDetailsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, entity.getId(), "=");
                Pair<Specification<ServiceDetails>, Pageable> pair = fetchData(listCommonRequest, ServiceDetails.class);
                Page<ServiceDetails> oldServiceDetails = serviceDetailsDao.findAll(pair.getLeft(), pair.getRight());
                List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(jsonHelper.convertValueToList(serviceDetailsRequestList, ServiceDetails.class), id, oldServiceDetails.stream().toList());
                entity.setServicesList(updatedServiceDetails);
            }
            if (shipmentAddressesRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT_ADDRESSES);
                Pair<Specification<Parties>, Pageable> pair = fetchData(listCommonRequest, Parties.class);
                Page<Parties> oldParties = partiesDao.findAll(pair.getLeft(), pair.getRight());
                List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(jsonHelper.convertValueToList(shipmentAddressesRequestList, Parties.class), id, Constants.SHIPMENT_ADDRESSES, oldParties.stream().toList());
                entity.setShipmentAddresses(updatedParties);
            }
            if (notesRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT);
                Pair<Specification<Notes>, Pageable> pair = fetchData(listCommonRequest, Notes.class);
                Page<Notes> oldNoteList = notesDao.findAll(pair.getLeft(), pair.getRight());
                List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(jsonHelper.convertValueToList(notesRequestList, Notes.class), id, Constants.SHIPMENT, oldNoteList.stream().toList());
                entity.setNotesList(updatedNotes);
            }
            if (customerBookingNotes != null && customerBookingNotes.size() > 0) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.CUSTOMER_BOOKING);
                Pair<Specification<Notes>, Pageable> pair = fetchData(listCommonRequest, Notes.class);
                Page<Notes> oldNoteList = notesDao.findAll(pair.getLeft(), pair.getRight());
                if(oldNoteList == null || oldNoteList.isEmpty()) {
                    List<Notes> updatedNotes = notesDao.saveEntityFromOtherEntity(jsonHelper.convertValueToList(customerBookingNotes, Notes.class), id, Constants.CUSTOMER_BOOKING);
                }
            }
            if(!dataMigration)
                pushShipmentDataToDependentService(entity, isCreate);
            ShipmentDetailsResponse response = jsonHelper.convertValue(entity, ShipmentDetailsResponse.class);

            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new ValidationException(e.getMessage());
        }
    }

    private void createV1AuditLogs(Long id, List<AuditLogRequestV2> auditLogRequestV2) {
        try {
            syncEntityConversionService.auditLogsV1ToV2(auditLogRequestV2, id);
        } catch (Exception ignored) {
            log.error("Error while migrating audit logs for id: " + id);
        }
    }

    private void createAuditLog(ShipmentDetails entity, String oldEntityJsonString, String operation)
    {
        try {
            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(entity)
                            .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, ShipmentDetails.class) : null)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(entity.getId())
                            .operation(operation).build()
            );
        }
        catch (Exception e) {
            log.error("Error creating audit service log", e);
        }
    }

    public ResponseEntity<IRunnerResponse> calculateContainerSummary(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        CalculateContainerSummaryRequest request = (CalculateContainerSummaryRequest) commonRequestModel.getData();
        try {
            List<Containers> containers = jsonHelper.convertValueToList(request.getContainersList(), Containers.class);
            ContainerSummaryResponse response = containerService.calculateContainerSummary(containers, request.getTransportMode(), request.getShipmentType());
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
            PackSummaryResponse response = packingService.calculatePackSummary(packingList, request.getTransportMode(), request.getShipmentType(), new ShipmentMeasurementDetailsDto());
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
    public ResponseEntity<IRunnerResponse> getAllMasterData(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            Long id = commonRequestModel.getId();
            Optional<ShipmentDetails> shipmentDetailsOptional = shipmentDao.findById(id);
            if(!shipmentDetailsOptional.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_ID_ERROR, id);
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ShipmentDetails shipmentDetails = shipmentDetailsOptional.get();
            ShipmentDetailsResponse shipmentDetailsResponse = jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class);
            Map<String, Object> response = fetchAllMasterDataByKey(shipmentDetails, shipmentDetailsResponse);
            if(shipmentDetails.getClient() != null && StringUtility.isNotEmpty(shipmentDetails.getClient().getOrgCode())) {
                fetchCreditLimitMasterData(shipmentDetails.getClient().getOrgCode(), shipmentDetails.getClient().getAddressCode(), response);
            }
            return ResponseHelper.buildSuccessResponse(response);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    /**
     * * fetchAllMasterDataByKey to be used for direct API
     * @param shipmentDetails
     * @param shipmentDetailsResponse
     * @return
     */
    public Map<String, Object> fetchAllMasterDataByKey(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllUnlocationDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCarrierDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCurrencyDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCommodityTypesInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllWarehouseDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var activityDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllActivityDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var salesAgentFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllSalesAgentInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllContainerTypesInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var vesselsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllVesselDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        var dgSubstanceFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllDGSubstanceDataInSingleCall(shipmentDetails, shipmentDetailsResponse, masterDataResponse)), executorService);
        CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture, activityDataFuture, salesAgentFuture,
                containerTypeFuture, vesselsFuture, dgSubstanceFuture).join();

        return masterDataResponse;
    }

    /**
     * * createShipmentPayload to be used while retrieving shipment
     * @param shipmentDetails
     * @param shipmentDetailsResponse
     */
    public void createShipmentPayload(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        try {
            double _start = System.currentTimeMillis();
            var masterListFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllMasterDataInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var unLocationsFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllUnlocationDataInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var carrierFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCarrierDataInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var currencyFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCurrencyDataInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var commodityTypesFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllCommodityTypesInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllTenantDataInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var wareHouseDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllWarehouseDataInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var activityDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllActivityDataInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var salesAgentFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllSalesAgentInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addAllContainerTypesInSingleCall(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            // TODO- Remove this call and sync job staus from billing using producer and consumer
            var billDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> this.addBillData(shipmentDetails, shipmentDetailsResponse, null)), executorService);
            CompletableFuture.allOf(masterListFuture, unLocationsFuture, carrierFuture, currencyFuture, commodityTypesFuture, tenantDataFuture, wareHouseDataFuture, activityDataFuture, salesAgentFuture,
                    containerTypeFuture, billDataFuture).join();
            Map<Long, ContainerResponse> map = new HashMap<>();
            List<ContainerResponse> containers = shipmentDetailsResponse.getContainersList();
            if(containers != null)
                map = containers.stream().collect(Collectors.toMap(ContainerResponse::getId, Function.identity()));
            setContainersPacksAutoUpdateData(shipmentDetailsResponse, map);
            setTruckDriverDetailsData(shipmentDetailsResponse, map);
            if(!Objects.isNull(shipmentDetails)) {
                shipmentDetailsResponse.setPackSummary(packingService.calculatePackSummary(shipmentDetails.getPackingList(), shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType(), new ShipmentMeasurementDetailsDto()));
                shipmentDetailsResponse.setContainerSummary(containerService.calculateContainerSummary(shipmentDetails.getContainersList(), shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType()));
            }
            try {
                if(shipmentDetailsResponse.getId() != null) {
                    var awb = awbDao.findByShipmentId(shipmentDetailsResponse.getId());
                    if (awb != null && !awb.isEmpty()) {
                        if (awb.get(0).getAirMessageStatus() != null)
                            shipmentDetailsResponse.setAwbStatus(awb.get(0).getAirMessageStatus());
                        else
                            shipmentDetailsResponse.setAwbStatus(AwbStatus.AWB_GENERATED);
                    }
                }
                if(!shipmentDetailsResponse.getAdditionalDetails().getIsSummaryUpdated())
                    shipmentDetailsResponse.getAdditionalDetails().setSummary(shipmentDetailsResponse.getContainerSummary().getSummary());
            } catch (Exception e) {}
            if(!Objects.isNull(shipmentDetails)) {
                List<ConsolidationDetails> consolidationList = shipmentDetails.getConsolidationList();
                if(!Objects.isNull(consolidationList) && !consolidationList.isEmpty()){
                    List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationList.get(0).getId());
                    if(!Objects.isNull(consoleShipmentMappings) && !consoleShipmentMappings.isEmpty())
                        shipmentDetailsResponse.setShipmentCount((long) consoleShipmentMappings.size());
                    else
                        shipmentDetailsResponse.setShipmentCount(0L);
                } else {
                    shipmentDetailsResponse.setShipmentCount(0L);
                }
            } else {
                shipmentDetailsResponse.setShipmentCount(0L);
            }
            log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_RETRIEVE_COMPLETE_MASTER_DATA, (System.currentTimeMillis() - _start) , LoggerHelper.getRequestIdFromMDC());
        }
        catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_RETRIEVE, ex.getLocalizedMessage());
        }

    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {

        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<MasterListRequest> listRequests = new ArrayList<>(masterDataUtils.createInBulkMasterListRequest(shipmentDetailsResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName() ));
        if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName() ));
        if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
            listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() ));

        if(masterDataResponse != null) {
            if(!Objects.isNull(shipmentDetailsResponse.getRoutingsList()))
                shipmentDetailsResponse.getRoutingsList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getBookingCarriagesList()))
                shipmentDetailsResponse.getBookingCarriagesList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, BookingCarriage.class, fieldNameKeyMap, BookingCarriage.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getPackingList()))
                shipmentDetailsResponse.getPackingList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getReferenceNumbersList()))
                shipmentDetailsResponse.getReferenceNumbersList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, ReferenceNumbers.class, fieldNameKeyMap, ReferenceNumbers.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getServicesList()))
                shipmentDetailsResponse.getServicesList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, ServiceDetails.class, fieldNameKeyMap, ServiceDetails.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getContainersList()))
                shipmentDetailsResponse.getContainersList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId())));
        }

        MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
        masterListRequestV2.setMasterListRequests(listRequests);
        masterListRequestV2.setIncludeCols(Arrays.asList("ItemType", "ItemValue", "ItemDescription", "ValuenDesc", "Cascade"));

        Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
        masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST);

        if(masterDataResponse == null) {
            shipmentDetailsResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName()), CacheConstants.MASTER_LIST));
            if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                shipmentDetailsResponse.getAdditionalDetails().setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.MASTER_LIST) );
            if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
                shipmentDetailsResponse.getCarrierDetails().setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.MASTER_LIST) );
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllUnlocationDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> locationCodes = new ArrayList<>();
        if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
            locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() )));
        if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName() )));

        if(masterDataResponse != null) {
            if(!Objects.isNull(shipmentDetailsResponse.getRoutingsList()))
                shipmentDetailsResponse.getRoutingsList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getBookingCarriagesList()))
                shipmentDetailsResponse.getBookingCarriagesList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, BookingCarriage.class, fieldNameKeyMap, BookingCarriage.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getContainersList()))
                shipmentDetailsResponse.getContainersList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getServicesList()))
                shipmentDetailsResponse.getServicesList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, ServiceDetails.class, fieldNameKeyMap, ServiceDetails.class.getSimpleName() + r.getId())));
            if(!Objects.isNull(shipmentDetailsResponse.getPackingList()))
                shipmentDetailsResponse.getPackingList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId())));
        }

        Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
        masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS);

        if(masterDataResponse == null) {
            if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
                shipmentDetailsResponse.getCarrierDetails().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS));
            if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                shipmentDetailsResponse.getAdditionalDetails().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllTenantDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> tenantIdList = new ArrayList<>(masterDataUtils.createInBulkTenantsRequest(shipmentDetailsResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName()));
        if(!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            tenantIdList.addAll(masterDataUtils.createInBulkTenantsRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName()));

        Map v1Data = masterDataUtils.fetchInTenantsList(tenantIdList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.TENANTS);

        if(masterDataResponse == null) {
            shipmentDetailsResponse.setTenantIdsData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName()), CacheConstants.TENANTS));
            if(!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                shipmentDetailsResponse.getAdditionalDetails().setTenantIdsData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.TENANTS));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.TENANTS, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCurrencyDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> currencyList = new ArrayList<>(masterDataUtils.createInBulkCurrencyRequest(shipmentDetailsResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName()));
        Map v1Data = masterDataUtils.fetchInCurrencyList(currencyList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.CURRENCIES);

        if(masterDataResponse == null) {
            shipmentDetailsResponse.setCurrenciesMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName()), CacheConstants.CURRENCIES));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CURRENCIES, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCarrierDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> carrierList = new ArrayList<>();
        if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
            carrierList = new ArrayList<>(masterDataUtils.createInBulkCarriersRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName()));

        if(masterDataResponse != null) {
            if (!Objects.isNull(shipmentDetailsResponse.getRoutingsList())) {
                List<String> finalCarrierList = carrierList;
                shipmentDetailsResponse.getRoutingsList().forEach(r -> finalCarrierList.addAll(masterDataUtils.createInBulkCarriersRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId() )));
            }
        }

        Map v1Data = masterDataUtils.fetchInBulkCarriers(carrierList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER);

        if(masterDataResponse == null) {
            shipmentDetailsResponse.getCarrierDetails().setCarrierMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.CARRIER));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CARRIER, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCommodityTypesInSingleCall(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> commodityTypes = new HashSet<>();
        if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
            shipmentDetailsResponse.getContainersList().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId() )));

        if(masterDataResponse != null) {
            if (!Objects.isNull(shipmentDetailsResponse.getPackingList()))
                shipmentDetailsResponse.getPackingList().forEach(r -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId() )));
        }

        Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY);

        if(masterDataResponse == null) {
            if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
                shipmentDetailsResponse.getContainersList().forEach(r -> r.setCommodityTypeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.COMMODITY)));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COMMODITY, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllWarehouseDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> wareHouseTypes = new HashSet<>();
        if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            wareHouseTypes.addAll(masterDataUtils.createInBulkWareHouseRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName()) );

        Map v1Data = masterDataUtils.fetchInWareHousesList(wareHouseTypes.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.WAREHOUSES);

        if(masterDataResponse == null) {
            if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                shipmentDetailsResponse.getAdditionalDetails().addTextData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.WAREHOUSES));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.WAREHOUSES, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllActivityDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> activityTypes = new HashSet<>();
        if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            activityTypes.addAll(masterDataUtils.createInBulkActivityTypeRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName()) );

        Map v1Data = masterDataUtils.fetchInActivityMasterList(activityTypes.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.ACTIVITY_TYPE);

        if(masterDataResponse == null) {
            if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
                shipmentDetailsResponse.getAdditionalDetails().addTextData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.ACTIVITY_TYPE));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.ACTIVITY_TYPE, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllSalesAgentInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> salesAgents = new HashSet<>();
        if (!Objects.isNull(shipmentDetailsResponse))
            salesAgents.addAll(masterDataUtils.createInBulkSalesAgentRequest(shipmentDetailsResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName()) );

        Map v1Data = masterDataUtils.fetchInSalesAgentList(salesAgents.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.SALES_AGENT);

        if(masterDataResponse == null) {
            if (!Objects.isNull(shipmentDetailsResponse))
                shipmentDetailsResponse.addTextData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName()), CacheConstants.SALES_AGENT));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.SALES_AGENT, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllContainerTypesInSingleCall(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> containerTypes = new ArrayList<>();
        if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
            shipmentDetailsResponse.getContainersList().forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId() )));

        Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
        masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE);

        if(masterDataResponse == null) {
            if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
                shipmentDetailsResponse.getContainersList().forEach(r -> r.setContainerCodeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.CONTAINER_TYPE)));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CONTAINER_TYPE, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllVesselDataInSingleCall(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> vesselList = new ArrayList<>();
        if (!Objects.isNull(shipmentDetailsResponse.getBookingCarriagesList()))
            shipmentDetailsResponse.getBookingCarriagesList().forEach(r -> vesselList.addAll(masterDataUtils.createInBulkVesselsRequest(r, BookingCarriage.class, fieldNameKeyMap, BookingCarriage.class.getSimpleName() + r.getId() )));
        if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
            vesselList.addAll((masterDataUtils.createInBulkVesselsRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() )));
        if (!Objects.isNull(shipmentDetailsResponse.getRoutingsList()))
            shipmentDetailsResponse.getRoutingsList().forEach(r -> vesselList.addAll(masterDataUtils.createInBulkVesselsRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId() )));

        Map v1Data = masterDataUtils.fetchInBulkVessels(vesselList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.VESSELS);

        if(masterDataResponse == null) {
            shipmentDetailsResponse.getCarrierDetails().setVesselsMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.VESSELS));
        }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.VESSELS, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(Arrays.asList()));
    }

    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllDGSubstanceDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> dgSubstanceIdList = new HashSet<>();
        if (!Objects.isNull(shipmentDetailsResponse.getPackingList()))
            shipmentDetailsResponse.getPackingList().forEach(r -> dgSubstanceIdList.addAll(masterDataUtils.createInBulkDGSubstanceRequest(r, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + r.getId() )));

        Map v1Data = masterDataUtils.fetchInDGSubstanceList(dgSubstanceIdList.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.DG_SUBSTANCES);

        if(masterDataResponse == null) { }
        else {
            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.DG_SUBSTANCES, masterDataResponse);
        }

        return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
    }

    private CompletableFuture<ShipmentBillingListResponse> addBillData(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse, Map<String, Object> masterDataResponse) {
        if (Objects.isNull(shipmentDetails) || Objects.isNull(TenantSettingsDetailsContext.getCurrentTenantSettings()) || Boolean.TRUE.equals(TenantSettingsDetailsContext.getCurrentTenantSettings().getBillingServiceV2Enabled()))
            return CompletableFuture.completedFuture(null);
        ShipmentBillingListRequest shipmentBillingListRequest = ShipmentBillingListRequest.builder()
                .guidsList(List.of(shipmentDetails.getGuid())).build();
        ShipmentBillingListResponse shipmentBillingListResponse = v1Service.fetchShipmentBillingData(shipmentBillingListRequest);
        if (shipmentBillingListResponse != null && shipmentBillingListResponse.getData() != null && shipmentBillingListResponse.getData().containsKey(shipmentDetails.getGuid().toString())
                && !IsStringNullOrEmpty(shipmentBillingListResponse.getData().get(shipmentDetails.getGuid().toString()).getBillStatus())) {
            shipmentDetails.setJobStatus(shipmentBillingListResponse.getData().get(shipmentDetails.getGuid().toString()).getBillStatus());
            shipmentDetailsResponse.setJobStatus(shipmentBillingListResponse.getData().get(shipmentDetails.getGuid().toString()).getBillStatus());
            shipmentDao.saveJobStatus(shipmentDetails.getId(), shipmentDetails.getJobStatus());
        }
        return CompletableFuture.completedFuture(shipmentBillingListResponse);
    }

    private void setContainersPacksAutoUpdateData (ShipmentDetailsResponse shipmentDetailsResponse, Map<Long, ContainerResponse> map) {
        List<PackingResponse> packings = shipmentDetailsResponse.getPackingList();
        List<ContainerResponse> containers = shipmentDetailsResponse.getContainersList();
        Map<Long, Map<String, String>> contMap = new HashMap<>();
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        boolean flag = shipmentDetailsResponse.getContainerAutoWeightVolumeUpdate() != null && shipmentDetailsResponse.getContainerAutoWeightVolumeUpdate().booleanValue()
                && shipmentSettingsDetails.getMultipleShipmentEnabled() != null && shipmentSettingsDetails.getMultipleShipmentEnabled();
        if(packings != null && packings.size() > 0) {
            for (PackingResponse pack : packings) {
                if(pack.getContainerId() != null) {
                    if(map.containsKey(pack.getContainerId())) {
                        pack.setContainerNumber(map.get(pack.getContainerId()).getContainerNumber());
                        pack.setContainerDesc(String.format("%s-%s-%s", map.get(pack.getContainerId()).getContainerCount(), map.get(pack.getContainerId()).getContainerNumber(), map.get(pack.getContainerId()).getContainerCode()));
                    }
                    if(flag) {
                        if(!contMap.containsKey(pack.getContainerId())) {
                            Map<String, String> tempMap = new HashMap<>();
                            tempMap.put(Constants.HANDLING_INFO, "");
                            tempMap.put(Constants.DESCRIPTION_OF_GOODS, "");
                            contMap.put(pack.getContainerId(), tempMap);
                        }
                        String handlingInfo = contMap.get(pack.getContainerId()).get(Constants.HANDLING_INFO);
                        String descriptionOfGoods = contMap.get(pack.getContainerId()).get(Constants.DESCRIPTION_OF_GOODS);
                        if(!IsStringNullOrEmpty(pack.getHandlingInfo())) {
                            if (handlingInfo.length() == 0)
                                handlingInfo = pack.getHandlingInfo();
                            else
                                handlingInfo = handlingInfo + ", " + pack.getHandlingInfo();
                        }
                        if(!IsStringNullOrEmpty(pack.getGoodsDescription())) {
                            if(descriptionOfGoods.length() == 0)
                                descriptionOfGoods = pack.getGoodsDescription();
                            else
                                descriptionOfGoods = descriptionOfGoods + ", " + pack.getGoodsDescription();
                        }
                        contMap.get(pack.getContainerId()).put(Constants.HANDLING_INFO, handlingInfo);
                        contMap.get(pack.getContainerId()).put(Constants.DESCRIPTION_OF_GOODS, descriptionOfGoods);
                    }
                }
            }
        }
        if(containers != null && containers.size() > 0) {
            for(ContainerResponse container : containers) {
                if(flag) {
                    if(contMap.containsKey(container.getId())) {
                        container.setTextFieldData(contMap.get(container.getId()));
                    }
                    else {
                        Map<String, String> tempMap = new HashMap<>();
                        tempMap.put(Constants.HANDLING_INFO, "");
                        tempMap.put(Constants.DESCRIPTION_OF_GOODS, "");
                        container.setTextFieldData(tempMap);
                    }
                }
                else {
                    Map<String, String> tempMap = new HashMap<>();
                    tempMap.put(Constants.HANDLING_INFO, container.getHandlingInfo());
                    tempMap.put(Constants.DESCRIPTION_OF_GOODS, container.getDescriptionOfGoods());
                    container.setTextFieldData(tempMap);
                }
            }
        }
    }

    private void setTruckDriverDetailsData(ShipmentDetailsResponse shipmentDetailsResponse, Map<Long, ContainerResponse> map) {
        List<TruckDriverDetailsResponse> truckDriverDetailsResponses = shipmentDetailsResponse.getTruckDriverDetails();
        if(truckDriverDetailsResponses != null && !truckDriverDetailsResponses.isEmpty()) {
            for (TruckDriverDetailsResponse truckDriverDetailsResponse: truckDriverDetailsResponses) {
                if(truckDriverDetailsResponse.getContainerId() != null && map.containsKey(truckDriverDetailsResponse.getContainerId())) {
                    truckDriverDetailsResponse.setContainerNumber(map.get(truckDriverDetailsResponse.getContainerId()).getContainerNumber());
                }
            }
        }
    }

    public ResponseEntity<IRunnerResponse> cloneShipment(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_RETRIEVE_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ShipmentRequest cloneShipmentDetails = jsonHelper.convertValue(shipmentDetails.get(), ShipmentRequest.class);
            cloneShipmentDetails.setHouseBill(null);
            cloneShipmentDetails.setBookingNumber(null);
            cloneShipmentDetails.setContainersList(null);
            cloneShipmentDetails.setRoutingsList(null);
            cloneShipmentDetails.setShipmentId(null);
            cloneShipmentDetails.setMasterBill(null);
            cloneShipmentDetails.setConsolidationList(null);
            cloneShipmentDetails.setStatus(ShipmentStatus.Created.getValue());
            cloneShipmentDetails.setConsolRef(null);
            cloneShipmentDetails.setEventsList(null);
            cloneShipmentDetails.setBookingReference(null);
            cloneShipmentDetails.setSourceGuid(null);
            cloneShipmentDetails.setClonedGuid(shipmentDetails.get().getGuid());
            cloneShipmentDetails.setContractId(null);
            cloneShipmentDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));

            cloneShipmentDetails.setShipmentCreatedOn(LocalDateTime.now());

            if(Constants.TRANSPORT_MODE_SEA.equals(cloneShipmentDetails.getTransportMode()) && Constants.DIRECTION_EXP.equals(cloneShipmentDetails.getDirection()) && !Constants.SHIPMENT_TYPE_DRT.equals(cloneShipmentDetails.getJobType()))
                cloneShipmentDetails.setHouseBill(generateCustomHouseBL(null));

            CommonRequestModel requestModel = CommonRequestModel.buildRequest(cloneShipmentDetails);
            log.info("Shipment details cloning started for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ShipmentDetailsResponse response = jsonHelper.convertValue(cloneShipmentDetails, ShipmentDetailsResponse.class);
            addAllUnlocationDataInSingleCall(null, response, null);
            addAllTenantDataInSingleCall(null, response, null);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> transportInstructionList(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            TIListRequest tiListRequest = (TIListRequest) commonRequestModel.getData();
            if(tiListRequest == null) {
                log.error("Request is empty for TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(tiListRequest.getShipmentGuid() == null) {
                log.error("Shipment Guid is null for TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            V1DataResponse v1DataResponse = v1Service.fetchTransportInstructionList(tiListRequest);
            List<TIResponse> tiResponseList = jsonHelper.convertValueToList(v1DataResponse.entities, TIResponse.class);
            return ResponseHelper.buildSuccessResponse(tiResponseList);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> containerListForTI(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            TIContainerListRequest tiContainerListRequest = (TIContainerListRequest) commonRequestModel.getData();
            if(tiContainerListRequest == null) {
                log.error("Request is empty for container TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(tiContainerListRequest.getShipmentGuid() == null) {
                log.error("Shipment Guid is null for conatiner TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            V1DataResponse v1DataResponse = v1Service.fetchContainersListForTI(tiContainerListRequest);
            List<TIContainerResponse> containerResponseList = jsonHelper.convertValueToList(v1DataResponse.entities, TIContainerResponse.class);
            return ResponseHelper.buildSuccessResponse(containerResponseList);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveByOrderId(String orderId) throws RunnerException {
        try {
            ShipmentDetailsResponse response = jsonHelper.convertValue(orderManagementAdapter.getOrder(orderId), ShipmentDetailsResponse.class);
            this.createShipmentPayload(null, response);
            this.addAllMasterDataInSingleCall(null, response, null);
            this.addAllUnlocationDataInSingleCall(null, response, null);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e){
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> generateCustomHouseBLNumber() throws RunnerException {
        try {
            return ResponseHelper.buildSuccessResponse(GenerateCustomHblResponse.builder().hblNumber(generateCustomHouseBL(null)).build());
        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getShipmentFromConsol(Long consolidationId, String bookingNumber) {
        var tenantSettings = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        // Populate shipment details on basis of tenant settings

        ShipmentDetailsResponse shipment;
        var consolidationResponse = consolidationDetailsDao.findById(consolidationId);

        if (consolidationResponse.isEmpty())
            throw new DataRetrievalFailureException("Failed to fetch the consolidation with id " + consolidationId);

        var consolidation = modelMapper.map(consolidationResponse.get(), ConsolidationDetailsResponse.class);

        var origin = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getOrigin() : null;
        var destination = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getDestination() : null;
        var originPort = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getOriginPort() : null;
        var destinationPort = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getDestinationPort() : null;
        var voyage = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getVoyage() : null;
        var vessel = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getVessel() : null;
        var aircrafType = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getAircraftType() : null;
        var eta = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getEta() : null;
        var etd = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getEtd() : null;
        var ata = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getAta() : null;
        var atd = consolidation.getCarrierDetails() != null ? consolidation.getCarrierDetails().getAtd() : null;
        var consolAllocation = consolidation.getAllocations();
        var consolCarrier = consolidation.getCarrierDetails();
        shipment = ShipmentDetailsResponse.builder()
                .transportMode(consolidation.getTransportMode() == null ? tenantSettings.getDefaultTransportMode() : consolidation.getTransportMode())
                .bookingNumber(bookingNumber)
                .consolidationList(List.of(modelMapper.map(consolidation, ConsolidationListResponse.class)))
                .direction(consolidation.getShipmentType() == null ? tenantSettings.getDefaultShipmentType() : consolidation.getShipmentType())
                .jobType(Constants.SHIPMENT_TYPE_STD)
                .shipmentType(consolidation.getContainerCategory() == null ? tenantSettings.getDefaultContainerType() : consolidation.getContainerCategory())
                .additionalDetails(AdditionalDetailResponse.builder()
                        .SMTPIGMDate(consolidation.getSmtpigmDate())
                        .SMTPIGMNumber(consolidation.getSmtpigmNumber())
                        .inwardDateAndTime(consolidation.getInwardDateAndTime())
                        .releaseType(consolidation.getReleaseType())
                        .warehouseId(consolidation.getWarehouseId())
                        .isInland(consolidation.getIsInland())
                        .original(consolidation.getOriginal())
                        .IGMFileNo(consolidation.getIgmFileNo())
                        .IGMFileDate(consolidation.getIgmFileDate())
                        .IGMInwardDate(consolidation.getIgmInwardDate())
                        .copy(consolidation.getCopy())
                        .customDeclType(consolidation.getDeclarationType())
                        .importBroker(consolidation.getReceivingAgent())
                        .exportBroker(consolidation.getSendingAgent())
                        .build())
                .carrierDetails(CarrierDetailResponse.builder()
                        .ata(ata)
                        .eta(eta)
                        .atd(atd)
                        .etd(etd)
                        .origin(origin)
                        .vessel(vessel)
                        .originPort(originPort)
                        .destinationPort(destinationPort)
                        .shippingLine(consolCarrier != null ? consolCarrier.getShippingLine() : null)
                        .voyage(voyage)
                        .aircraftType(aircrafType)
                        .destination(destination)
                        .flightNumber(consolCarrier != null ? consolCarrier.getFlightNumber() : null)
                        .build())
                .weight(consolAllocation != null ? consolAllocation.getWeight() : null)
                .weightUnit(consolAllocation != null ? consolAllocation.getWeightUnit() : tenantSettings.getWeightChargeableUnit())
                .volume(consolAllocation != null ? consolAllocation.getVolume() : null)
                .volumeUnit(consolAllocation != null ? consolAllocation.getVolumeUnit() : tenantSettings.getVolumeChargeableUnit())
                .chargable(consolAllocation != null ? consolAllocation.getChargable() : null)
                .chargeableUnit(consolAllocation != null ? consolAllocation.getChargeableUnit() : null)
                .paymentTerms(consolidation.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && consolidation.getShipmentType().equals("EXP")
                        ? consolidation.getPayment() : null)
                .masterBill(consolidation.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) ? consolidation.getMawb() : consolidation.getBol())
                .status(0)
                .source(Constants.SYSTEM)
                .createdBy(UserContext.getUser().getUsername())
                .customerCategory(CustomerCategoryRates.CATEGORY_5)
                .shipmentCreatedOn(LocalDateTime.now())
                .consolRef(consolidation.getConsolidationNumber())
                .build();

        if (consolidation.getConsolidationAddresses() != null) {
            consolidation.getConsolidationAddresses().stream().forEach(party -> {
                if (party.getType().equals("NP1")) {
                    shipment.getAdditionalDetails().setNotifyParty(
                            PartiesResponse.builder()
                                    .orgCode(party.getOrgCode())
                                    .addressCode(party.getAddressCode())
                                    .build());
                }
            });
        }

        if(!IsStringNullOrEmpty(shipment.getCarrierDetails().getOrigin())) {
            if(IsStringNullOrEmpty(shipment.getAdditionalDetails().getPaidPlace()))
                shipment.getAdditionalDetails().setPaidPlace(shipment.getCarrierDetails().getOrigin());
            if(IsStringNullOrEmpty(shipment.getAdditionalDetails().getPlaceOfIssue()))
                shipment.getAdditionalDetails().setPlaceOfIssue(shipment.getCarrierDetails().getOrigin());
            if(IsStringNullOrEmpty(shipment.getAdditionalDetails().getPlaceOfSupply()))
                shipment.getAdditionalDetails().setPlaceOfSupply(shipment.getCarrierDetails().getOrigin());
        }
        if(shipment.getCarrierDetails().getEta() != null) {
            if(shipment.getAdditionalDetails().getDateOfIssue() == null)
                shipment.getAdditionalDetails().setDateOfIssue(shipment.getCarrierDetails().getEta());
            if(shipment.getAdditionalDetails().getDateOfReceipt() == null)
                shipment.getAdditionalDetails().setDateOfReceipt(shipment.getCarrierDetails().getEta());
        }

        PartiesResponse parties;
        if(consolidation.getReceivingAgent() != null) {
            parties = jsonHelper.convertValue(consolidation.getReceivingAgent(), PartiesResponse.class);
            parties.setId(null);
            parties.setGuid(null);
            shipment.getAdditionalDetails().setImportBroker(parties);
        }
        if(consolidation.getSendingAgent() != null) {
            parties = jsonHelper.convertValue(consolidation.getSendingAgent(), PartiesResponse.class);
            parties.setId(null);
            parties.setGuid(null);
            shipment.getAdditionalDetails().setExportBroker(parties);
        }

        //Generate HBL
        if(Constants.TRANSPORT_MODE_SEA.equals(shipment.getTransportMode()) && Constants.DIRECTION_EXP.equals(shipment.getDirection()))
            shipment.setHouseBill(generateCustomHouseBL(null));

        try {
            log.info("Fetching Tenant Model");
            TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
            String currencyCode = tenantModel.currencyCode;
            shipment.setFreightLocalCurrency(currencyCode);
        } catch (Exception e){
            log.error("Failed in fetching tenant data from V1 with error : {}", e);
        }

        createShipmentPayload(modelMapper.map(shipment, ShipmentDetails.class), shipment);

        return ResponseHelper.buildSuccessResponse(shipment);
    }

    public String generateCustomHouseBL(ShipmentDetails shipmentDetails) {
        String res = null;
        if(shipmentDetails != null) {
            res = shipmentDetails.getHouseBill();
        }
        ShipmentSettingsDetails tenantSetting = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        if(shipmentDetails == null && tenantSetting != null && tenantSetting.getRestrictHblGen()) {
            return null;
        }

        if (shipmentDetails != null && tenantSetting.getCustomisedSequence()) {
            try {
                res = productEngine.getCustomizedBLNumber(shipmentDetails, tenantSetting);
            } catch (Exception e) {
                log.error(e.getMessage());
            }
        }

        if(res == null || res.isEmpty()) {
            res = tenantSetting.getHousebillPrefix() ==  null ? "" : tenantSetting.getHousebillPrefix();
            String numberGeneration = tenantSetting.getHousebillNumberGeneration() ==  null ? "" : tenantSetting.getHousebillNumberGeneration();
            switch(numberGeneration) {
                case "Random" :
                    res += StringUtility.getRandomString(10);
                    break;
                case "Serial" :
                    String serialNumber = getShipmentsSerialNumber();
                    res += serialNumber;
                    break;
                default : res = "";
                    break;
            }
        }

        return res;
    }

    private String generateBlViaProductUtility(ShipmentDetails shipmentDetails, ShipmentSettingsDetails tenantSetting) {
        String res = null;
        try {
            res = productEngine.getCustomizedBLNumber(shipmentDetails, tenantSetting);
        } catch (Exception e) {
            log.error(e.getMessage());
        }
        return res;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getDefaultShipment() {
        String responseMsg;
        try {
            var tenantSettings = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
            // Populate shipment details on basis of tenant settings
            ShipmentDetailsResponse response = new ShipmentDetailsResponse();
            response.setAdditionalDetails(new AdditionalDetailResponse());
            response.setCarrierDetails(new CarrierDetailResponse());
            response.setTransportMode(tenantSettings.getDefaultTransportMode());
            response.setDirection(tenantSettings.getDefaultShipmentType());
            response.setShipmentType(tenantSettings.getDefaultContainerType());

            response.setVolumeUnit(tenantSettings.getVolumeChargeableUnit());
            response.setWeightUnit(tenantSettings.getWeightChargeableUnit());
            response.setStatus(0);
            response.setSource(Constants.SYSTEM);
            response.setCreatedBy(UserContext.getUser().getUsername());
            response.setCustomerCategory(CustomerCategoryRates.CATEGORY_5);
            response.setShipmentCreatedOn(LocalDateTime.now());
            response.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));
            //Generate HBL
            if(Constants.TRANSPORT_MODE_SEA.equals(response.getTransportMode()) && Constants.DIRECTION_EXP.equals(response.getDirection()))
                response.setHouseBill(generateCustomHouseBL(null));

            try {
                log.info("Fetching Tenant Model");
                TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
                String currencyCode = tenantModel.currencyCode;
                response.setFreightLocalCurrency(currencyCode);
                List<UnlocationsResponse> unlocationsResponse = masterDataUtils.fetchUnlocationByOneIdentifier(EntityTransferConstants.ID, StringUtility.convertToString(tenantModel.getUnloco()));
                if (!Objects.isNull(unlocationsResponse) && !unlocationsResponse.isEmpty()) {
                    response.getAdditionalDetails().setPlaceOfIssue(unlocationsResponse.get(0).getLocationsReferenceGUID());
                    response.getAdditionalDetails().setPaidPlace(unlocationsResponse.get(0).getLocationsReferenceGUID());
                    response.getAdditionalDetails().setPlaceOfSupply(unlocationsResponse.get(0).getLocationsReferenceGUID());
                }
            } catch (Exception e){
                log.error("Failed in fetching tenant data from V1 with error : {}", e);
            }

            if(Constants.TRANSPORT_MODE_SEA.equals(response.getTransportMode()) && Constants.DIRECTION_EXP.equals(response.getDirection()))
                response.setHouseBill(generateCustomHouseBL(null));
            //response.setShipmentId(generateShipmentId());

            this.createShipmentPayload(null, response);

            return ResponseHelper.buildSuccessResponse(response);
        } catch(Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getMasterDataMappings() {
        String responseMsg;
        try {
            List<MasterDataDescriptionResponse> response = new ArrayList<>();

            //Get current Tenant's setting
            Optional<ShipmentSettingsDetails> optional = shipmentSettingsDao.findByTenantId(TenantContext.getCurrentTenant());
            var tenantSetting = optional.get();
            // get all the master data based on field names of
            response = masterDataUtils.getMasterDataDescription(tenantSetting);

            return  ResponseHelper.buildSuccessResponse(response);
        } catch(Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> attachListShipment(CommonRequestModel commonRequestModel){
        AttachListShipmentRequest request = (AttachListShipmentRequest) commonRequestModel.getData();

        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(request.getConsolidationId());
        if (!consolidationDetails.isPresent()) {
            log.debug("Consolidation Details is null for Id {} with Request Id {}", request.getConsolidationId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        request.setIncludeTbls(Arrays.asList(Constants.ADDITIONAL_DETAILS, Constants.CLIENT, Constants.CONSIGNER, Constants.CONSIGNEE, Constants.CARRIER_DETAILS, Constants.PICKUP_DETAILS, Constants.DELIVERY_DETAILS));
        ListCommonRequest listRequest = setCrieteriaForAttachShipment(request, consolidationDetails.get());
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(listRequest, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> spec = tuple.getLeft();
        if(shipmentSettingsDetails.getIsShipmentLevelContainer() != null && shipmentSettingsDetails.getIsShipmentLevelContainer())
            spec = spec.and(notInConsoleMappingTable());
        else
            spec = spec.and(notInConsoleMappingTable()).and(notInContainerMappingTable());

        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(spec , tuple.getRight());
        return ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements());
    }
    public static Specification<ShipmentDetails> notInConsoleMappingTable() {
        return (root, query, criteriaBuilder) -> {
            return criteriaBuilder.isEmpty(root.get(Constants.CONSOLIDATION_LIST));
        };
    }
    public static Specification<ShipmentDetails> notInContainerMappingTable() {
        return (root, query, criteriaBuilder) -> {
            return criteriaBuilder.isEmpty(root.get(Constants.CONTAINERS_LIST));
        };
    }

    private ListCommonRequest setCrieteriaForAttachShipment(AttachListShipmentRequest request, ConsolidationDetails consolidationDetails) {
        if(request.getFilterCriteria() != null && request.getFilterCriteria().isEmpty()){
            request.setFilterCriteria(Arrays.asList(FilterCriteria.builder().innerFilter(new ArrayList<>()).build()));
        }
        ListCommonRequest defaultRequest;
        defaultRequest = CommonUtils.andCriteria(Constants.TRANSPORT_MODE, consolidationDetails.getTransportMode(), "=", request);
        if(!Objects.isNull(consolidationDetails.getCarrierDetails().getOriginPort()))
            CommonUtils.andCriteria(Constants.ORIGIN_PORT, consolidationDetails.getCarrierDetails().getOriginPort(), "=", defaultRequest);
        else
            CommonUtils.andCriteria(Constants.ORIGIN_PORT, "", Constants.IS_NULL, defaultRequest);
        if(!Objects.isNull(consolidationDetails.getCarrierDetails().getDestinationPort()))
            CommonUtils.andCriteria(Constants.DESTINATION_PORT, consolidationDetails.getCarrierDetails().getDestinationPort(), "=", defaultRequest);
        else
            CommonUtils.andCriteria(Constants.DESTINATION_PORT, "", Constants.IS_NULL, defaultRequest);
        if(!Objects.isNull(consolidationDetails.getShipmentType()))
            CommonUtils.andCriteria(Constants.DIRECTION, consolidationDetails.getShipmentType(), "=", defaultRequest);
        else
            CommonUtils.andCriteria(Constants.DIRECTION, "", Constants.IS_NULL, defaultRequest);
        CommonUtils.andCriteria(Constants.STATUS, 2, "!=", defaultRequest);
        CommonUtils.andCriteria(Constants.STATUS, 3, "!=", defaultRequest);
        List<FilterCriteria> criterias = defaultRequest.getFilterCriteria();
        List<FilterCriteria> innerFilters = criterias.get(0).getInnerFilter();
        Criteria criteria = Criteria.builder().fieldName(Constants.TRANSPORT_MODE).operator("!=").value(Constants.TRANSPORT_MODE_AIR).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        List<FilterCriteria> innerFilers1 = new ArrayList<>();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName(Constants.JOB_TYPE).operator("!=").value(Constants.SHIPMENT_TYPE_DRT).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        criteria = Criteria.builder().fieldName(Constants.JOB_TYPE).operator(Constants.IS_NULL).build();
        filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
        innerFilers1.add(filterCriteria);
        filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
        innerFilters.add(filterCriteria);

        if(request.getEtaMatch()){
            innerFilers1 = new ArrayList<>();
            if(!Objects.isNull(consolidationDetails.getCarrierDetails().getEta()))
                criteria = Criteria.builder().fieldName("eta").operator("=").value(consolidationDetails.getCarrierDetails().getEta()).build();
            else
                criteria = Criteria.builder().fieldName("eta").operator(Constants.IS_NULL).build();
            filterCriteria = FilterCriteria.builder().criteria(criteria).build();
            innerFilers1.add(filterCriteria);
            criteria = Criteria.builder().fieldName("eta").operator(Constants.IS_NULL).build();
            filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
            innerFilers1.add(filterCriteria);
            filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
            innerFilters.add(filterCriteria);
        }
        if(request.getEtdMatch()){
            innerFilers1 = new ArrayList<>();
            if(!Objects.isNull(consolidationDetails.getCarrierDetails().getEtd()))
                criteria = Criteria.builder().fieldName("etd").operator("=").value(consolidationDetails.getCarrierDetails().getEtd()).build();
            else
                criteria = Criteria.builder().fieldName("etd").operator(Constants.IS_NULL).build();
            filterCriteria = FilterCriteria.builder().criteria(criteria).build();
            innerFilers1.add(filterCriteria);
            criteria = Criteria.builder().fieldName("etd").operator(Constants.IS_NULL).build();
            filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
            innerFilers1.add(filterCriteria);
            filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
            innerFilters.add(filterCriteria);
        }
        if(request.getScheduleMatch()){
            if(Objects.equals(consolidationDetails.getTransportMode(),Constants.TRANSPORT_MODE_AIR)){
                innerFilers1 = new ArrayList<>();
                if(!Objects.isNull(consolidationDetails.getCarrierDetails().getFlightNumber()))
                    criteria = Criteria.builder().fieldName(Constants.FLIGHT_NUMBER).operator("=").value(consolidationDetails.getCarrierDetails().getFlightNumber()).build();
                else
                    criteria = Criteria.builder().fieldName(Constants.FLIGHT_NUMBER).operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).build();
                innerFilers1.add(filterCriteria);
                criteria = Criteria.builder().fieldName(Constants.FLIGHT_NUMBER).operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
                innerFilers1.add(filterCriteria);
                filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
                innerFilters.add(filterCriteria);

                innerFilers1 = new ArrayList<>();
                if(!Objects.isNull(consolidationDetails.getCarrierDetails().getShippingLine()))
                    criteria = Criteria.builder().fieldName(Constants.SHIPPING_LINE).operator("=").value(consolidationDetails.getCarrierDetails().getShippingLine()).build();
                else
                    criteria = Criteria.builder().fieldName(Constants.SHIPPING_LINE).operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).build();
                innerFilers1.add(filterCriteria);
                criteria = Criteria.builder().fieldName(Constants.SHIPPING_LINE).operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
                innerFilers1.add(filterCriteria);
                filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
                innerFilters.add(filterCriteria);
            }
            else if(Objects.equals(consolidationDetails.getTransportMode(), Constants.TRANSPORT_MODE_SEA)){
                innerFilers1 = new ArrayList<>();
                if(!Objects.isNull(consolidationDetails.getCarrierDetails().getVessel()))
                    criteria = Criteria.builder().fieldName(Constants.VESSEL).operator("=").value(consolidationDetails.getCarrierDetails().getVessel()).build();
                else
                    criteria = Criteria.builder().fieldName(Constants.VESSEL).operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).build();
                innerFilers1.add(filterCriteria);
                criteria = Criteria.builder().fieldName(Constants.VESSEL).operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
                innerFilers1.add(filterCriteria);
                filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
                innerFilters.add(filterCriteria);

                innerFilers1 = new ArrayList<>();
                if(!Objects.isNull(consolidationDetails.getCarrierDetails().getVoyage()))
                    criteria = Criteria.builder().fieldName(Constants.VOYAGE).operator("=").value(consolidationDetails.getCarrierDetails().getVoyage()).build();
                else
                    criteria = Criteria.builder().fieldName(Constants.VOYAGE).operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).build();
                innerFilers1.add(filterCriteria);
                criteria = Criteria.builder().fieldName(Constants.VOYAGE).operator(Constants.IS_NULL).build();
                filterCriteria = FilterCriteria.builder().criteria(criteria).logicOperator("or").build();
                innerFilers1.add(filterCriteria);
                filterCriteria = FilterCriteria.builder().logicOperator("and").innerFilter(innerFilers1).build();
                innerFilters.add(filterCriteria);
            }
        }
        return defaultRequest;
    }

    /**
     * back flows data of the current updated shipment to all its sibling shipments attached to the common console
     * @param current_shipment
     * @param old_shipment
     */
    private ConsolidationDetails updateLinkedShipmentData(ShipmentDetails shipment, ShipmentDetails oldEntity) throws RunnerException {
        List<ConsolidationDetails> consolidationList = shipment.getConsolidationList();
        ConsolidationDetails consolidationDetails;
        var linkedConsol = (consolidationList != null && consolidationList.size() > 0) ? consolidationList.get(0) : null;
        if(linkedConsol != null && Objects.equals(shipment.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(shipment.getAdditionalDetails().getEfreightStatus(), Constants.NON)){
            consolidationDetails = consolidationDetailsDao.findById(linkedConsol.getId()).get();
            if(consolidationDetails != null && Objects.equals(consolidationDetails.getEfreightStatus(), Constants.EAW)){
                throw new RunnerException("EFreight status can only be EAW as Consolidation EFrieght Status is EAW");
            }
        }
        if(linkedConsol != null && (oldEntity == null || !Objects.equals(shipment.getMasterBill(),oldEntity.getMasterBill()) ||
                !Objects.equals(shipment.getDirection(),oldEntity.getDirection()) ||
                (shipment.getCarrierDetails() != null && oldEntity.getCarrierDetails() != null &&
                (!Objects.equals(shipment.getCarrierDetails().getVoyage(),oldEntity.getCarrierDetails().getVoyage()) ||
                        !Objects.equals(shipment.getCarrierDetails().getVessel(),oldEntity.getCarrierDetails().getVessel()) ||
                        !Objects.equals(shipment.getCarrierDetails().getShippingLine(),oldEntity.getCarrierDetails().getShippingLine()) ||
                        !Objects.equals(shipment.getCarrierDetails().getAircraftType(),oldEntity.getCarrierDetails().getAircraftType())
                )))) {
            consolidationDetails = consolidationDetailsDao.findById(consolidationList.get(0).getId()).get();
            consolidationDetails.setBol(shipment.getMasterBill());
            if(consolidationDetails.getCarrierDetails() == null)
                consolidationDetails.setCarrierDetails(new CarrierDetails());
            consolidationDetails.getCarrierDetails().setAircraftType(shipment.getCarrierDetails().getAircraftType());
            consolidationDetails.getCarrierDetails().setShippingLine(shipment.getCarrierDetails().getShippingLine());
            consolidationDetails.getCarrierDetails().setVessel(shipment.getCarrierDetails().getVessel());
            consolidationDetails.getCarrierDetails().setVoyage(shipment.getCarrierDetails().getVoyage());
            consolidationDetails.setShipmentType(shipment.getDirection());
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationDetails.getId());
            List<Long> shipmentIdList = consoleShipmentMappings.stream().filter(c -> !Objects.equals(c.getShipmentId(), shipment.getId()))
                        .map(i -> i.getShipmentId()).toList();
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false);
            if (!shipmentIdList.isEmpty()) {
                ListCommonRequest listReq = constructListCommonRequest("id", shipmentIdList, "IN");
                Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listReq, ShipmentDetails.class, tableNames);
                Page<ShipmentDetails> page = shipmentDao.findAll(pair.getLeft(), pair.getRight());

                List<ShipmentDetails> shipments = page.getContent();
                shipments.stream()
                    .map(i -> {
                        i.setMasterBill(shipment.getMasterBill());
                        i.setDirection(shipment.getDirection());
                        if (shipment.getCarrierDetails() != null) {
                            i.getCarrierDetails().setVoyage(shipment.getCarrierDetails().getVoyage());
                            i.getCarrierDetails().setVessel(shipment.getCarrierDetails().getVessel());
                            i.getCarrierDetails().setShippingLine(shipment.getCarrierDetails().getShippingLine());
                            i.getCarrierDetails().setAircraftType(shipment.getCarrierDetails().getAircraftType());
                        }
                        return i;
                    }).toList();
                shipmentDao.saveAll(shipments);
            }
            return consolidationDetails;
        }
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getIdFromGuid(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_RETRIEVE_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getGuid() == null) {
                log.error("Request Guid Id is null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(UUID.fromString(request.getGuid()));
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Shipment details fetched successfully for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(ShipmentDetailsResponse.builder().id(shipmentDetails.get().getId()).build());
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
                log.error(ShipmentConstants.SHIPMENT_RETRIEVE_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(request.getId());
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Shipment details fetched successfully for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(ShipmentDetailsResponse.builder().guid(shipmentDetails.get().getGuid()).build());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void autoGenerateEvents(ShipmentDetails shipmentDetails, Integer previousStauts) {
        Events response = null;
        if(shipmentDetails.getStatus() != null) {
            if (previousStauts == null || !shipmentDetails.getStatus().equals(previousStauts)) {
                if (shipmentDetails.getStatus().equals(ShipmentStatus.Confirmed.getValue())) {
                    response = createAutomatedEvents(shipmentDetails, Constants.SHPCNFRM);
                }
                if (shipmentDetails.getStatus().equals(ShipmentStatus.Completed.getValue())) {
                    response = createAutomatedEvents(shipmentDetails, Constants.SHPCMPLT);
                }
            }
            if(response != null) {
                if (shipmentDetails.getEventsList() == null)
                    shipmentDetails.setEventsList(new ArrayList<>());
                shipmentDetails.getEventsList().add(response);
            }
        }
    }

    private void autoGenerateCreateEvent(ShipmentDetails shipmentDetails) {
        Events response = null;
        response = createAutomatedEvents(shipmentDetails, Constants.SHPCRTD);

        if(response != null) {
            if (shipmentDetails.getEventsList() == null)
                shipmentDetails.setEventsList(new ArrayList<>());
            shipmentDetails.getEventsList().add(response);
        }
    }

    private Events createAutomatedEvents(ShipmentDetails shipmentDetails, String eventCode) {
        Events events = new Events();
        // Set event fields from shipment
        events.setActual(LocalDateTime.now());
        events.setEstimated(LocalDateTime.now());
        events.setSource(Constants.CARGO_RUNNER);
        events.setIsPublicTrackingEvent(true);
        events.setEntityType(Constants.SHIPMENT);
        events.setEntityId(shipmentDetails.getId());
        events.setTenantId(TenantContext.getCurrentTenant());
        events.setEventCode(eventCode);
        // Persist the event
        eventDao.save(events);
        return events;
    }

    public ResponseEntity<IRunnerResponse> fetchShipmentsForConsoleId(CommonRequestModel commonRequestModel) throws RunnerException {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        if(request.getId() == null) {
            log.error("Request Id is null for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Id can't be null");
        }
        Long id = request.getId();
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(id);
        List<Long> shipmentIdsList = new ArrayList<>();
        if(consoleShipmentMappings != null && consoleShipmentMappings.size() > 0) {
            shipmentIdsList = consoleShipmentMappings.stream().map(x -> x.getShipmentId()).toList();
        }
        ListCommonRequest listCommonRequest = CommonUtils.andCriteria("id", shipmentIdsList, "IN", null);
        return fetchShipments(CommonRequestModel.buildRequest(listCommonRequest));
    }

    public ResponseEntity<IRunnerResponse> fetchActiveInvoices(CommonRequestModel commonRequestModel) throws RunnerException {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        if(request.getGuid() == null) {
            log.error("Request guid is null for fetch active invoices with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException("Shipment Guid can't be null");
        }
        CheckActiveInvoiceRequest checkActiveInvoiceRequest = CheckActiveInvoiceRequest.builder().BillGuid(request.getGuid()).build();
        return ResponseHelper.buildSuccessResponse(v1Service.getActiveInvoices(checkActiveInvoiceRequest));
    }


    private boolean shipmentHasHblOrHawb(ShipmentDetails shipmentDetails) {
        boolean res = false;
        if(shipmentDetails.getTransportMode() == null || shipmentDetails.getDirection() == null)
            return false;
        if(shipmentDetails.getTransportMode().equals("AIR") && shipmentDetails.getDirection().equals("EXP") && (
                shipmentDetails.getJobType() != null && shipmentDetails.getJobType().equals("STD")))
            res = true;
        if(shipmentDetails.getTransportMode().equals("SEA") && shipmentDetails.getDirection().equals("EXP"))
            res = true;
        return res;
    }

    public ResponseEntity<IRunnerResponse> showAssignAllContainers(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ShipmentConsoleIdDto request = (ShipmentConsoleIdDto) commonRequestModel.getData();
            Long shipmentId = request.getShipmentId();
            Long consolidationId = request.getConsolidationId();
            List<ShipmentsContainersMapping> shipmentsContainersMappingList = shipmentsContainersMappingDao.findByShipmentId(shipmentId);
            List<Containers> containers = containerDao.findByConsolidationId(consolidationId);
            boolean showDialog = false;
            if(shipmentsContainersMappingList != null && containers != null && containers.size() > 0 &&
                    containers.size() != shipmentsContainersMappingList.size())
            {
                ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant())).get(0);
                if(shipmentSettingsDetails.getMultipleShipmentEnabled() == null || !shipmentSettingsDetails.getMultipleShipmentEnabled()) {
                    for (Containers containers1 : containers) {
                        if(containers1.getShipmentsList() == null || containers1.getShipmentsList().size() == 0) {
                            showDialog = true;
                            break;
                        }
                    }
                }
                else
                    showDialog = true;
            }
            int numberOfShipments = 0;
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationId);
            if(consoleShipmentMappings != null && consoleShipmentMappings.size() > 0)
                numberOfShipments = consoleShipmentMappings.size();
            AssignAllDialogDto response = new AssignAllDialogDto();
            response.setShowDialog(showDialog);
            response.setNumberOfShipments(numberOfShipments);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> fetchCreditLimit(String orgCode, String addressCode) throws RunnerException {
        if(StringUtility.isEmpty(orgCode)) {
            throw new RunnerException("OrgCode to fetch creditLimit can't be null");
        }
        AddressTranslationRequest.OrgAddressCode orgAddressCode = AddressTranslationRequest.OrgAddressCode.builder().OrgCode(orgCode).AddressCode(addressCode).build();
        try {
            V1DataResponse v1DataResponse = v1Service.fetchCreditLimit(orgAddressCode);
            if(v1DataResponse.entities == null) {
                log.debug(ShipmentConstants.NO_DATA_FOUND_FOR_ORG_CODE, orgCode);
                return ResponseHelper.buildSuccessResponse();
            }
            List<CreditLimitResponse> creditLimitResponses = jsonHelper.convertValueToList(v1DataResponse.getEntities(), CreditLimitResponse.class);
            if(creditLimitResponses == null || creditLimitResponses.size() == 0) {
                log.debug(ShipmentConstants.NO_DATA_FOUND_FOR_ORG_CODE, orgCode);
                return ResponseHelper.buildSuccessResponse();
            }
            return ResponseHelper.buildDependentServiceResponse(creditLimitResponses.get(0), 0, 0);
        } catch (Exception e) {
            log.debug("No Data found for org code {} {}", orgCode, e.getMessage());
        }

        return ResponseHelper.buildSuccessResponse();
    }

    public void fetchCreditLimitMasterData(String orgCode, String addressCode, Map<String, Object> response) {
        if(StringUtility.isEmpty(orgCode)) {
            return;
        }
        AddressTranslationRequest.OrgAddressCode orgAddressCode = AddressTranslationRequest.OrgAddressCode.builder().OrgCode(orgCode).AddressCode(addressCode).build();
        try {
            V1DataResponse v1DataResponse = v1Service.fetchCreditLimit(orgAddressCode);
            if(v1DataResponse.entities == null) {
                log.debug(ShipmentConstants.NO_DATA_FOUND_FOR_ORG_CODE, orgCode);
                return ;
            }
            List<CreditLimitResponse> creditLimitResponses = jsonHelper.convertValueToList(v1DataResponse.getEntities(), CreditLimitResponse.class);
            if(creditLimitResponses == null || creditLimitResponses.size() == 0) {
                log.debug(ShipmentConstants.NO_DATA_FOUND_FOR_ORG_CODE, orgCode);
                return;
            }
            if(response == null) {
                response = new HashMap<>();
            }
            response.put(Constants.CREDIT_LIMIT, creditLimitResponses.get(0));
        } catch (Exception e) {
            log.debug("No Data found for org code {} {}", orgCode, e.getMessage());
        }
    }

    @Override
    public void updateDateAndStatus(Long id, LocalDateTime date, Integer status) throws RunnerException {
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
        if(shipmentDetails.isPresent()) {
            ShipmentDetails shipment = shipmentDetails.get();
//            if(date != null) {
//                shipment.getAdditionalDetails().setDateOfIssue(date);
//            }
            if(status != null) {
                shipment.setStatus(status);
            }
            shipmentDao.save(shipment, false);
            try {
                shipmentSync.sync(shipment, null, null, shipment.getGuid().toString(), false);
            } catch (Exception e) {
                log.error(SyncingConstants.ERROR_SYNCING_SHIPMENTS, e);
            }
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchEmails(Long shipmentId, Long consolidationId) {
        if(Objects.isNull(shipmentId) && Objects.isNull(consolidationId)) {
            log.error("Invalid request for fetchEmails");
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }
        if (!Objects.isNull(shipmentId)) {
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(shipmentId);
            if (shipmentDetails.isEmpty())
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            return v1ServiceUtil.fetchEmailIdsForShipment(shipmentDetails.get());
        }
        else if (!Objects.isNull(consolidationId)) {
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(consolidationId);
            if (consolidationDetails.isEmpty())
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            return v1ServiceUtil.fetchEmailIdsForConsolidation(consolidationDetails.get());
        }
        return ResponseHelper.buildFailedResponse(DaoConstants.DAO_INVALID_REQUEST_MSG);
    }

    @Override
    public ResponseEntity<IRunnerResponse> checkCreditLimitFromV1(CommonRequestModel commonRequestModel){
        CheckCreditLimitFromV1Request request = (CheckCreditLimitFromV1Request) commonRequestModel.getData();
        String checkCreditLimitDocs = IReport.checkCreditLimitDocs(request.getDocType());
        if(!Objects.isNull(checkCreditLimitDocs)){
            Optional<ShipmentDetails> shipmentsRow = shipmentDao.findById(request.getShipmentId());
            ShipmentDetails shipmentDetails = null;
            if(shipmentsRow.isPresent()) {
                shipmentDetails = shipmentsRow.get();
                var response = v1ServiceUtil.validateCreditLimit(modelMapper.map(shipmentDetails.getClient(), Parties.class), checkCreditLimitDocs, shipmentDetails.getGuid(), request.getTaskCreation());
                return ResponseHelper.buildSuccessResponse(response);
            }
            return ResponseHelper.buildFailedResponse("Shipment not exist for given id");
        } else {
            return ResponseHelper.buildFailedResponse("Please send a valid doc type for check credit limit.");
        }
    }
}
