package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.Kafka.Dto.KafkaResponse;
import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.adapters.impl.OrderManagementAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerPartialListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.ContainerAPIsRequest.ShipmentContainerAssignRequest;
import com.dpw.runner.shipment.services.dto.TrackingService.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.patchRequest.ShipmentPatchRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.request.ShipmentBillingListRequest;
import com.dpw.runner.shipment.services.dto.v1.request.TIListRequest;
import com.dpw.runner.shipment.services.dto.v1.request.WayBillNumberFilterRequest;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.mapper.ShipmentDetailsMapper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IHblService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.AzureServiceBusTopic;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.SBUtilsImpl;
import com.dpw.runner.shipment.services.syncing.impl.ShipmentSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.utils.*;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
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
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ShipmentService implements IShipmentService {
    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private ShipmentDetailsMapper shipmentDetailsMapper;

    private final CSVParsingUtil<ShipmentDetails> parser = new CSVParsingUtil<>(ShipmentDetails.class);

    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private ICarrierDao carrierDao;
    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private IAdditionalDetailDao additionalDetailDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IBookingCarriageDao bookingCarriageDao;

    @Autowired
    private IELDetailsDao elDetailsDao;

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private IFileRepoDao fileRepoDao;

    @Autowired
    private IJobDao jobDao;

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
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private IConsolidationService consolidationService;

    @Autowired
    private SBUtilsImpl sbUtils;

    @Autowired
    private ISBProperties isbProperties;

    @Autowired
    private AzureServiceBusTopic azureServiceBusTopic;
    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private AuditLogService auditLogService;
    @Autowired
    ShipmentSync shipmentSync;
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
    private OrderManagementAdapter orderManagementAdapter;

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

    private ShipmentDetails currentShipment;

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
            Map.entry("clientOrgCode", RunnerEntityMapping.builder().tableName("client").dataType(String.class).fieldName("orgCode").isContainsText(true).build()),
            Map.entry("consignerOrgCode", RunnerEntityMapping.builder().tableName("consigner").dataType(String.class).fieldName("orgCode").isContainsText(true).build()),
            Map.entry("consigneeOrgCode", RunnerEntityMapping.builder().tableName("consignee").dataType(String.class).fieldName("orgCode").isContainsText(true).build()),
            Map.entry("clientAddressCode", RunnerEntityMapping.builder().tableName("client").dataType(Integer.class).fieldName("addressCode").isContainsText(true).build()),
            Map.entry("consignerAddressCode", RunnerEntityMapping.builder().tableName("consigner").dataType(String.class).fieldName("addressCode").isContainsText(true).build()),
            Map.entry("consigneeAddressCode", RunnerEntityMapping.builder().tableName("consignee").dataType(String.class).fieldName("addressCode").isContainsText(true).build()),
            Map.entry("houseBill", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("houseBill").isContainsText(true).build()),
            Map.entry("houseBillType", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("houseBillType").isContainsText(true).build()),
            Map.entry("transportMode", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("transportMode").isContainsText(true).build()),
            Map.entry("releaseType", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("releaseType").isContainsText(true).build()),
            Map.entry("deliveryMode", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("deliveryMode").isContainsText(true).build()),
            Map.entry("direction", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("direction").isContainsText(true).build()),
            Map.entry("shipmentType", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("shipmentType").isContainsText(true).build()),
            Map.entry("status", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).fieldName("status").build()),
            Map.entry("guid", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(UUID.class).fieldName("guid").build()),
            Map.entry("source", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("source").isContainsText(true).build()),
            Map.entry("jobType", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("jobType").isContainsText(true).build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("createdBy").isContainsText(true).build()),
            Map.entry("serviceType", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("serviceType").isContainsText(true).build()),
            Map.entry("masterBill", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("masterBill").isContainsText(true).build()),
            Map.entry("bookingReference", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("bookingReference").isContainsText(true).build()),
            Map.entry("consolRef", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("consolRef").isContainsText(true).build()),
            Map.entry("salesAgent", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Long.class).fieldName("salesAgent").build()),
            Map.entry("paymentTerms", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("paymentTerms").isContainsText(true).build()),
            Map.entry("incoterms", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("incoterms").isContainsText(true).build()),
            Map.entry("shipmentId", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("shipmentId").isContainsText(true).build()),
            Map.entry("isDomestic", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Boolean.class).fieldName("isDomestic").build()),
            Map.entry("assignedTo", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).fieldName("assignedTo").build()),
            Map.entry("additionalTerms", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("additionalTerms").isContainsText(true).build()),
            Map.entry("goodsDescription", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("goodsDescription").isContainsText(true).build()),
            Map.entry("createdAt", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(LocalDateTime.class).fieldName("createdAt").build()),
            Map.entry("updatedAt", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(LocalDateTime.class).fieldName("updatedAt").build()),
            Map.entry("deliveryEstimated", RunnerEntityMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).fieldName("estimatedPickupOrDelivery").build()),
            Map.entry("deliveryActual", RunnerEntityMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).fieldName("actualPickupOrDelivery").build()),
            Map.entry("deliveryRequiredBy", RunnerEntityMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).fieldName("requiredBy").build()),
            Map.entry("pickupEstimated", RunnerEntityMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).fieldName("estimatedPickupOrDelivery").build()),
            Map.entry("pickupActual", RunnerEntityMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).fieldName("actualPickupOrDelivery").build()),
            Map.entry("pickupRequiredBy", RunnerEntityMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).fieldName("requiredBy").build()),
            Map.entry("screeningStatus", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("screeningStatus").isContainsText(true).build()),
            Map.entry("paidPlace", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(Long.class).fieldName("paidPlace").build()),
            Map.entry("placeOfIssue", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(Long.class).fieldName("placeOfIssue").build()),
            Map.entry("dateOfIssue", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(LocalDateTime.class).fieldName("dateOfIssue").build()),
            Map.entry("dateOfReceipt", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(LocalDateTime.class).fieldName("dateOfReceipt").build()),
            Map.entry("goodsCo", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("goodsCo").build()),
            Map.entry("BOEDate", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(LocalDateTime.class).fieldName("BOEDate").build()),
            Map.entry("boeNumber", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("BOENumber").isContainsText(true).build()),
            Map.entry("shippingLine", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("shippingLine").isContainsText(true).build()),
            Map.entry("vessel", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("vessel").build()),
            Map.entry("voyage", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("voyage").build()),
            Map.entry("origin", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("origin").build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("destination").build()),
            Map.entry("eta", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).fieldName("eta").build()),
            Map.entry("etd", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).fieldName("etd").build()),
            Map.entry("ata", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).fieldName("ata").build()),
            Map.entry("atd", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).fieldName("atd").build()),
            Map.entry("weight", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(BigDecimal.class).fieldName("weight").build()),
            Map.entry("weightUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("weightUnit").build()),
            Map.entry("volume", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(BigDecimal.class).fieldName("volume").build()),
            Map.entry("volumeUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("volumeUnit").build()),
            Map.entry("volumetricWeight", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(BigDecimal.class).fieldName("volumetricWeight").build()),
            Map.entry("volumetricWeightUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("volumetricWeightUnit").build()),
            Map.entry("chargable", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(BigDecimal.class).fieldName("chargable").build()),
            Map.entry("chargeableUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("chargeableUnit").build()),
            Map.entry("netWeight", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(BigDecimal.class).fieldName("netWeight").build()),
            Map.entry("netWeightUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("netWeightUnit").build()),
            Map.entry("noOfPacks", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).fieldName("noOfPacks").build()),
            Map.entry("packsUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("packsUnit").build()),
            Map.entry("innerPacks", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).fieldName("innerPacks").build()),
            Map.entry("innerPackUnit", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("innerPackUnit").build()),
            Map.entry("jobStatus", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("jobStatus").build()),
            Map.entry("containerNumber", RunnerEntityMapping.builder().tableName("containersList").dataType(String.class).fieldName("containerNumber").build()),
            Map.entry("containerCode", RunnerEntityMapping.builder().tableName("containersList").dataType(String.class).fieldName("containerCode").build()),
            Map.entry("id", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Long.class).fieldName("id").build()),
            Map.entry("consolidationNumber", RunnerEntityMapping.builder().tableName("consolidationList").dataType(String.class).fieldName("consolidationNumber").build()),
            Map.entry("orderNumber", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("orderManagementId").build()),
            Map.entry("referenceNumber", RunnerEntityMapping.builder().tableName("referenceNumbersList").dataType(String.class).fieldName("referenceNumber").build()),
            Map.entry("activityType", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("activityType").build()),
            Map.entry("goodsCO", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(String.class).fieldName("goodsCO").build()),
            Map.entry("route", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("route").build()),
            Map.entry("cargoFinanceBooking", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Boolean.class).fieldName("cargoFinanceBooking").build()),
            Map.entry("isCmsHBLSent", RunnerEntityMapping.builder().tableName("additionalDetails").dataType(Boolean.class).fieldName("isCmsHBLSent").build()),
            Map.entry("orderManagementId", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).fieldName("orderManagementId").isContainsText(true).build())
    );

    @Override
    @Transactional
    public List<ShipmentDetails> createTestShipment(Integer count) {
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
            CarrierDetails carrierDetail = createCarrier();
            shipmentDetail.setCarrierDetails(carrierDetail);

            shipmentDetail = shipmentDao.save(shipmentDetail, false);
            afterSave(shipmentDetail, true);
        }

        return response;
    }

    @Override
    @Transactional
    public ResponseEntity<?> fetchShipments(CommonRequestModel commonRequestModel) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        request.setIncludeTbls(Arrays.asList("additionalDetails", "client", "consigner", "consignee", "carrierDetails", "pickupDetails", "deliveryDetails"));
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
            masterDataUtils.setLocationData(responseList, EntityTransferConstants.UNLOCATION_CODE);
            masterDataUtils.setContainerTeuData(lst, responseList);
            setBillingData(lst, responseList);
        }
        catch (Exception ex) {
            log.error("Request: {} || Error occured for event: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
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

    private void setBillingData(List<ShipmentDetails> shipmentDetails, List<IRunnerResponse> responseList) {

        Map<Long, ShipmentListResponse> dataMap = new HashMap<>();
        for (IRunnerResponse response : responseList){
            dataMap.put(((ShipmentListResponse)response).getId(), (ShipmentListResponse)response);
        }

        if(shipmentDetails != null && shipmentDetails.size() > 0) {
            List<UUID> guidsList = new ArrayList<>();
            for (ShipmentDetails details: shipmentDetails) {
                guidsList.add(details.getGuid());
            }
            if(guidsList.size() > 0) {
                ShipmentBillingListRequest shipmentBillingListRequest = new ShipmentBillingListRequest();
                shipmentBillingListRequest.setGuidsList(guidsList);
                ShipmentBillingListResponse shipmentBillingListResponse = v1Service.fetchShipmentBillingData(shipmentBillingListRequest);
                if(shipmentBillingListResponse.getData() != null && !shipmentBillingListResponse.getData().isEmpty()) {
                    for (ShipmentDetails details: shipmentDetails) {
                        if(shipmentBillingListResponse.getData().get(details.getGuid()) != null) {
                            ShipmentBillingListResponse.BillingData billingData = shipmentBillingListResponse.getData().get(details.getGuid());
                            ShipmentListResponse shipmentListResponse = dataMap.get(details.getId());

                            shipmentListResponse.setBillStatus(billingData.getBillStatus());
                            shipmentListResponse.setJobStatus(billingData.getJobStatus());
                            shipmentListResponse.setTotalEstimatedCost(billingData.getTotalEstimatedCost());
                            shipmentListResponse.setTotalEstimatedRevenue(billingData.getTotalEstimatedRevenue());
                            shipmentListResponse.setTotalEstimatedProfit(billingData.getTotalEstimatedProfit());
                            shipmentListResponse.setTotalEstimatedProfitPercent(billingData.getTotalEstimatedProfitPercent());
                            shipmentListResponse.setTotalCost(billingData.getTotalCost());
                            shipmentListResponse.setTotalRevenue(billingData.getTotalRevenue());
                            shipmentListResponse.setTotalProfit(billingData.getTotalProfit());
                            shipmentListResponse.setTotalProfitPercent(billingData.getTotalProfitPercent());
                            shipmentListResponse.setTotalPostedCost(billingData.getTotalPostedCost());
                            shipmentListResponse.setTotalPostedRevenue(billingData.getTotalPostedRevenue());
                            shipmentListResponse.setTotalPostedProfit(billingData.getTotalPostedProfit());
                            shipmentListResponse.setTotalPostedProfitPercent(billingData.getTotalPostedProfitPercent());
                            shipmentListResponse.setWayBillNumber(billingData.getWayBillNumber());
                        }
                    }

                }
            }
        }
    }


    private List<Parties> createParties(ShipmentDetails shipmentDetails) {
        List<Parties> parties = new ArrayList<>();
        int random = new Random().nextInt(100);
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

    private CarrierDetails createCarrier() {
        int random = new Random().nextInt(100);
        CarrierDetails carrier = CarrierDetails.builder()
                .shippingLine(SHIPPING_LINE.get(random % SHIPPING_LINE.size()))
                .vessel(generateString(5)).voyage(generateString(5)).origin(LOCATIONS.get(random % LOCATIONS.size())).destination(LOCATIONS.get(random % LOCATIONS.size()))
                .eta(LocalDateTime.now()).etd(LocalDateTime.now()).ata(LocalDateTime.now()).atd(LocalDateTime.now())
                .build();
        carrier.setTenantId(1);
        return carrierDao.save(carrier);
    }


    private ShipmentDetails createShipmentData() {
        int random = new Random().nextInt(100);
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().direction(DIRECTIONS.get(random % DIRECTIONS.size())).status(1)
                .source(SOURCE.get(random % SOURCE.size())).transportMode(TRANSPORT_MODES.get(random % TRANSPORT_MODES.size())).shipmentType(SHIPMENT_TYPE.get(random % SHIPMENT_TYPE.size()))
                .houseBill(generateString(10)).masterBill(generateString(10)).bookingReference(generateString(10)).consolRef(generateString(10)).paymentTerms(generateString(3))
                .goodsDescription(generateString(10)).additionalTerms(generateString(10))
                .build();
        shipmentDetails.setTenantId(1);
        return shipmentDetails;
    }

    private String generateString(int length) {
        String SALTCHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";
        StringBuilder salt = new StringBuilder();
        Random rnd = new Random();
        while (salt.length() < length) {
            int index = (int) (rnd.nextFloat() * SALTCHARS.length());
            salt.append(SALTCHARS.charAt(index));
        }
        return salt.toString();
    }

    @Override
    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        //ExecutorService executorService = Executors.newFixedThreadPool(100);

        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Shipment Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        System.out.println(jsonHelper.convertToJson(request));

        ShipmentDetails shipmentDetails = jsonHelper.convertCreateValue(request, ShipmentDetails.class);
        AdditionalDetails additionalDetails = jsonHelper.convertCreateValue(request.getAdditionalDetails(), AdditionalDetails.class);
        CarrierDetails carrierDetails = jsonHelper.convertCreateValue(request.getCarrierDetails(), CarrierDetails.class);
        if(request.getConsolidationList() != null)
            shipmentDetails.setConsolidationList(jsonHelper.convertValueToList(request.getConsolidationList(), ConsolidationDetails.class));

        List<Long> tempConsolIds = new ArrayList<>();

        List<ConsolidationDetailsRequest> consolidationDetailsRequests = request.getConsolidationList();
        if (consolidationDetailsRequests != null) {
            for (ConsolidationDetailsRequest consolidation : consolidationDetailsRequests) {
                if (consolidation.getId() != null) {
                    tempConsolIds.add(consolidation.getId());
                }
            }
        }

        try {

//            if (request.getConsolidationList() != null) {
//                List<ConsolidationDetailsRequest> consolRequest = request.getConsolidationList();
//                List<ConsolidationDetails> consolList = consolidationDetailsDao.saveAll(commonUtils.convertToCreateEntityList(consolRequest, ConsolidationDetails.class));
//                shipmentDetails.setConsolidationList(consolList);
//            }
            List<Containers> updatedContainers = new ArrayList<>();
            if (request.getContainersList() != null) {
                List<ContainerRequest> containerRequest = request.getContainersList();
                updatedContainers = containerDao.saveAll(commonUtils.convertToCreateEntityList(containerRequest, Containers.class));
                shipmentDetails.setContainersList(updatedContainers);
            }

            shipmentDetails = getShipment(shipmentDetails);
            Long shipmentId = shipmentDetails.getId();

//            attachConsolidations(shipmentId, tempConsolIds);

            List<PackingRequest> packingRequest = request.getPackingList();
            List<Packing> updatedPackings = new ArrayList<>();
            if (packingRequest != null) {
                updatedPackings = packingDao.saveEntityFromShipment(commonUtils.convertToCreateEntityList(packingRequest, Packing.class), shipmentId);
                shipmentDetails.setPackingList(updatedPackings);
            }

            List<BookingCarriageRequest> bookingCarriageRequest = request.getBookingCarriagesList();
            if (bookingCarriageRequest != null)
                shipmentDetails.setBookingCarriagesList(bookingCarriageDao.saveEntityFromShipment(commonUtils.convertToCreateEntityList(bookingCarriageRequest, BookingCarriage.class), shipmentId));

            List<ELDetailsRequest> elDetailsRequest = request.getElDetailsList();
            if (elDetailsRequest != null)
                shipmentDetails.setElDetailsList(elDetailsDao.saveEntityFromShipment(commonUtils.convertToCreateEntityList(elDetailsRequest, ELDetails.class), shipmentId));

            List<EventsRequest> eventsRequest = request.getEventsList();
            if (eventsRequest != null)
                shipmentDetails.setEventsList(eventDao.saveEntityFromOtherEntity(commonUtils.convertToCreateEntityList(eventsRequest, Events.class), shipmentId, Constants.SHIPMENT));

            List<FileRepoRequest> fileRepoRequest = request.getFileRepoList();
            if (fileRepoRequest != null)
                shipmentDetails.setFileRepoList(fileRepoDao.saveEntityFromOtherEntity(commonUtils.convertToCreateEntityList(fileRepoRequest, FileRepo.class), shipmentId, Constants.SHIPMENT));

            List<JobRequest> jobRequest = request.getJobsList();
            if (jobRequest != null)
                shipmentDetails.setJobsList(jobDao.saveEntityFromShipment(commonUtils.convertToCreateEntityList(jobRequest, Jobs.class), shipmentId));

            List<NotesRequest> notesRequest = request.getNotesList();
            if (notesRequest != null)
                shipmentDetails.setNotesList(notesDao.saveEntityFromOtherEntity(commonUtils.convertToCreateEntityList(notesRequest, Notes.class), shipmentId, Constants.SHIPMENT));

            List<ReferenceNumbersRequest> referenceNumbersRequest = request.getReferenceNumbersList();
            if (referenceNumbersRequest != null)
                shipmentDetails.setReferenceNumbersList(referenceNumbersDao.saveEntityFromShipment(commonUtils.convertToCreateEntityList(referenceNumbersRequest, ReferenceNumbers.class), shipmentId));

            List<RoutingsRequest> routingsRequest = request.getRoutingsList();
            if (routingsRequest != null)
                shipmentDetails.setRoutingsList(routingsDao.saveEntityFromShipment(commonUtils.convertToCreateEntityList(routingsRequest, Routings.class), shipmentId));

            List<ServiceDetailsRequest> serviceDetailsRequest = request.getServicesList();
            if (serviceDetailsRequest != null)
                shipmentDetails.setServicesList(serviceDetailsDao.saveEntityFromShipment(commonUtils.convertToCreateEntityList(serviceDetailsRequest, ServiceDetails.class), shipmentId));

            List<PartiesRequest> shipmentAddressRequest = request.getShipmentAddresses();
            if (shipmentAddressRequest != null)
                shipmentDetails.setShipmentAddresses(partiesDao.saveEntityFromOtherEntity(commonUtils.convertToCreateEntityList(shipmentAddressRequest, Parties.class), shipmentId, Constants.SHIPMENT_ADDRESSES));

            try {
                shipmentSync.sync(shipmentDetails);
            } catch (Exception e){
                log.error("Error performing sync on shipment entity, {}", e);
            }

            if(updatedContainers.size() > 0) {
                hblService.checkAllContainerAssigned(shipmentId, updatedContainers, updatedPackings);
                if(tempConsolIds == null || tempConsolIds.size() == 0) {
                    createConsolidation(shipmentDetails, updatedContainers);
                }
            }
            afterSave(shipmentDetails, true);
//            EventMessage eventMessage = EventMessage.builder().messageType(Constants.SERVICE).entity(Constants.SHIPMENT).request(shipmentDetails).build();
//            sbUtils.sendMessagesToTopic(isbProperties, azureServiceBusTopic.getTopic(), Arrays.asList(new ServiceBusMessage(jsonHelper.convertToJsonIncludeNulls(eventMessage))));

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
            log.error(e.getMessage());
            //TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }


//        CompletableFuture.allOf(createCallToAdditionalDetails, createCallToContainers, createCallToPackings, createCallToBookingCarriages, createCallToElDetails, createCallToEvents, createCallToFileRepos, createCallToJobs, createCallToNotes, createCallToReferenceNumbers, createCallToRoutings, createCallToServiceDetails, createCallToPickupDelivery, createCallToParties, createCallToCarrierDetails).join();
//        executorService.shutdownNow();
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class));
    }

    ShipmentDetails getShipment(ShipmentDetails shipmentDetails) {
        if(shipmentDetails.getShipmentId() == null){
            this.currentShipment = shipmentDetails;
            shipmentDetails.setShipmentId(generateShipmentId());
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

    private void createJobsAsync(ShipmentDetails shipmentDetails, List<JobRequest> jobRequest) {
        jobRequest.forEach(jobs -> {
            createJob(shipmentDetails, jobs);
        });
    }

    private void createFileRepoAsync(ShipmentDetails shipmentDetails, List<FileRepoRequest> fileRepoRequest) {
        fileRepoRequest.forEach(fileRepo -> {
            createFileRepo(shipmentDetails, fileRepo);
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
    public void createFileRepo(ShipmentDetails shipmentDetails, FileRepoRequest fileRepoRequest) {
        fileRepoRequest.setEntityId(shipmentDetails.getId());
        fileRepoRequest.setEntityType(Constants.SHIPMENT);
        fileRepoDao.save(objectMapper.convertValue(fileRepoRequest, FileRepo.class));
    }

    @Transactional
    public void createJob(ShipmentDetails shipmentDetails, JobRequest jobRequest) {
        jobRequest.setShipmentId(shipmentDetails.getId());
        jobDao.save(objectMapper.convertValue(jobRequest, Jobs.class));
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

    @Transactional
    public void createCarrier(ShipmentDetails shipmentDetails, CarrierDetails carrierDetails) {
        carrierDao.save(carrierDetails);
    }


    public Optional<ShipmentDetails> retrieveByIdOrGuid(ShipmentRequest request){
        String responseMsg;

        if (request == null) {
            log.error("Request is empty for Shipment update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        Optional<ShipmentDetails> oldEntity = Optional.ofNullable(null);

        if(request.getId()!=null){
            long id = request.getId();
            oldEntity=shipmentDao.findById(id);
            if (!oldEntity.isPresent()) {
                log.debug("Shipment Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
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
    public ResponseEntity<?> createShipmentInV2(CustomerBookingRequest customerBookingRequest) throws Exception
    {
        ConsolidationDetailsRequest consolidationDetailsRequest = ConsolidationDetailsRequest.builder().
                        carrierDetails(customerBookingRequest.getCarrierDetails()).
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
                        build();

        consolidationService.create(CommonRequestModel.buildRequest(consolidationDetailsRequest));

        ShipmentRequest shipmentRequest = ShipmentRequest.builder().
                carrierDetails(customerBookingRequest.getCarrierDetails()).
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
                shipmentCreatedOn(customerBookingRequest.getBookingDate()).
                client(customerBookingRequest.getCustomer()).
                consignee(customerBookingRequest.getConsignee()).
                consigner(customerBookingRequest.getConsignor()).
                additionalDetails(AdditionalDetailRequest.builder().
                        notifyParty(customerBookingRequest.getNotifyParty()).
                        build()
                ).
                shipmentType(customerBookingRequest.getCargoType()).
                transportMode(customerBookingRequest.getTransportType()).
                direction(customerBookingRequest.getDirection()).
                jobType("STD").
                incoterms(customerBookingRequest.getIncoTerms()).
                serviceType(customerBookingRequest.getServiceMode()).
                status(1).
                fmcTlcId(customerBookingRequest.getFmcTlcId()).
                containersList(customerBookingRequest.getContainersList()).
                packingList(customerBookingRequest.getPackingList()).
                fileRepoList(customerBookingRequest.getFileRepoList()).
                routingsList(customerBookingRequest.getRoutingList()).
                build();

        return this.create(CommonRequestModel.buildRequest(shipmentRequest));
    }

    @Override
    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
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

        ShipmentDetails entity = objectMapper.convertValue(request, ShipmentDetails.class);
        entity.setId(oldEntity.get().getId());
        if(entity.getGuid() != null && !oldEntity.get().getGuid().equals(entity.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        if (entity.getContainersList() == null)
            entity.setContainersList(oldEntity.get().getContainersList());
        entity = shipmentDao.update(entity, false);
        afterSave(entity, false);
        try {
            shipmentSync.sync(entity);
        } catch (Exception e){
            log.error("Error performing sync on shipment entity, {}", e);
        }
        return ResponseHelper.buildSuccessResponse(objectMapper.convertValue(entity, ShipmentDetailsResponse.class));
    }

    @Transactional
    public ResponseEntity<?> attachConsolidations(Long shipmentId, List<Long> consolIds) {
        ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId).get();

        if (shipmentDetails != null) {
            for (Long consolId : consolIds) {
                ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(consolId).get();
                if (consolidationDetails != null) {
                    shipmentDetails.getConsolidationList().add(consolidationDetails);
                }
            }
            // TODO- remove this save
            ShipmentDetails entity = shipmentDao.save(shipmentDetails, false);
            return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(entity, ShipmentDetailsResponse.class));
        }

        return null;
    }

    @Transactional
    public ResponseEntity<?> completeUpdate(CommonRequestModel commonRequestModel) throws Exception {

        ShipmentRequest shipmentRequest = (ShipmentRequest) commonRequestModel.getData();

        List<BookingCarriageRequest> bookingCarriageRequestList = shipmentRequest.getBookingCarriagesList();
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

        // TODO- implement Validation logic

        Optional<ShipmentDetails> oldEntity = retrieveByIdOrGuid(shipmentRequest);
        long id=oldEntity.get().getId();
        if (!oldEntity.isPresent()) {
            log.debug("Shipment Details is null for Id {}", shipmentRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        List<Long> tempConsolIds = new ArrayList<>();

        List<ConsolidationDetailsRequest> consolidationDetailsRequests = shipmentRequest.getConsolidationList();
        if (consolidationDetailsRequests != null) {
            for (ConsolidationDetailsRequest consolidation : consolidationDetailsRequests) {
                if (consolidation.getId() != null) {
                    tempConsolIds.add(consolidation.getId());
                }
            }
        }
        else
            tempConsolIds = oldEntity.get().getConsolidationList().stream().map(e -> e.getId()).collect(Collectors.toList());

        try {
            List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
            ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
            if(shipmentSettingsDetailsList.size() > 0)
                shipmentSettingsDetails = shipmentSettingsDetailsList.get(0);
            ShipmentDetails entity = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
            entity.setId(oldEntity.get().getId());
            List<Containers> updatedContainers = new ArrayList<>();
            if (containerRequestList != null) {
                updatedContainers = containerDao.updateEntityFromShipmentConsole(convertToEntityList(containerRequestList, Containers.class), null);
            } else {
                updatedContainers = oldEntity.get().getContainersList();
            }
            entity.setContainersList(updatedContainers);

            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
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

            if (bookingCarriageRequestList != null) {
                List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(convertToEntityList(bookingCarriageRequestList, BookingCarriage.class), id);
                entity.setBookingCarriagesList(updatedBookingCarriages);
            }
            List<Packing> updatedPackings = new ArrayList<>();
            if (packingRequestList != null) {
                updatedPackings = packingDao.updateEntityFromShipment(convertToEntityList(packingRequestList, Packing.class), id);
                entity.setPackingList(updatedPackings);
            }
            if (elDetailsRequestList != null) {
                List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(convertToEntityList(elDetailsRequestList, ELDetails.class), id);
                entity.setElDetailsList(updatedELDetails);
            }
            if (eventsRequestList != null) {
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(convertToEntityList(eventsRequestList, Events.class), id, Constants.SHIPMENT);
                entity.setEventsList(updatedEvents);
            }
            if (jobRequestList != null) {
                List<Jobs> updatedJobs = jobDao.updateEntityFromShipment(convertToEntityList(jobRequestList, Jobs.class), id);
                entity.setJobsList(updatedJobs);
            }
            if (referenceNumbersRequestList != null) {
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id);
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (routingsRequestList != null) {
                List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(convertToEntityList(routingsRequestList, Routings.class), id);
                entity.setRoutingsList(updatedRoutings);
            }
            if (serviceDetailsRequestList != null) {
                List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(convertToEntityList(serviceDetailsRequestList, ServiceDetails.class), id);
                entity.setServicesList(updatedServiceDetails);
            }
            if (fileRepoRequestList != null) {
                List<FileRepo> updatedFileRepos = fileRepoDao.updateEntityFromOtherEntity(convertToEntityList(fileRepoRequestList, FileRepo.class), id, Constants.SHIPMENT);
                entity.setFileRepoList(updatedFileRepos);
            }
            if (notesRequestList != null) {
                List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(convertToEntityList(notesRequestList, Notes.class), id, Constants.SHIPMENT);
                entity.setNotesList(updatedNotes);
            }

            if (shipmentAddressList != null) {
                List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(convertToEntityList(shipmentAddressList, Parties.class), id, Constants.SHIPMENT_ADDRESSES);
                entity.setShipmentAddresses(updatedParties);
            }

            try {
                shipmentSync.sync(entity);
            } catch (Exception e){
                log.error("Error performing sync on shipment entity, {}", e);
            }
            if(updatedContainers.size() > 0) {
                hblService.checkAllContainerAssigned(id, updatedContainers, updatedPackings);
                if((tempConsolIds == null || tempConsolIds.size() == 0) && (shipmentSettingsDetails.getIsShipmentLevelContainer() == null || !shipmentSettingsDetails.getIsShipmentLevelContainer())) {
                    createConsolidation(entity, updatedContainers);
                }
            }
            afterSave(entity, false);
            ShipmentDetailsResponse response = shipmentDetailsMapper.map(entity);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RuntimeException(e.getMessage());
        }
    }

    public void afterSave(ShipmentDetails shipmentDetails, boolean isCreate) {
        try {
            if(shipmentDetails.getTenantId() == null)
                shipmentDetails.setTenantId(TenantContext.getCurrentTenant());
            if(shipmentDetails.getStatus() != null && !Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Completed.getValue()) || shipmentDetails.getStatus() != null && !Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Cancelled.getValue())) {
                if (trackingServiceAdapter.checkIfConsolAttached(shipmentDetails)|| (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getShipmentType().equals(Constants.SHIPMENT_TYPE_DRT) && !Objects.isNull(shipmentDetails.getHouseBill()))) {
                    UniversalTrackingPayload _utPayload = trackingServiceAdapter.mapShipmentDataToTrackingServiceData(shipmentDetails);
                    List<UniversalTrackingPayload> trackingPayloads = new ArrayList<>();
                    if(_utPayload != null) {
                        trackingPayloads.add(_utPayload);
                        var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                        trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody, false);
                    }
                }
            }
            if(shipmentDetails.getSource() != null && shipmentDetails.getSource().equals(Constants.API)) {
                var events = trackingServiceAdapter.getAllEvents(shipmentDetails,null);
                var universalEventsPayload = trackingServiceAdapter.mapEventDetailsForTracking(shipmentDetails.getBookingReference(),Constants.SHIPMENT, shipmentDetails.getShipmentId(), events);
                List<UniversalTrackingPayload.UniversalEventsPayload> trackingPayloads= new ArrayList<>();
                if(universalEventsPayload != null) {
                    trackingPayloads.add(universalEventsPayload);
                    var jsonBody = jsonHelper.convertToJson(trackingPayloads);
                    trackingServiceAdapter.publishUpdatesToTrackingServiceQueue(jsonBody,true);
                }
            }
            KafkaResponse kafkaResponse = producer.getKafkaResponse(shipmentDetails, isCreate);
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, UUID.randomUUID().toString());
        }
        catch (Exception e) {
            log.error(e.getMessage());
        }
    }

    public void createConsolidation(ShipmentDetails shipmentDetails, List<Containers> containers) {
        List<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
        ShipmentSettingsDetails shipmentSettings = null;
        if(shipmentSettingsDetails != null && shipmentSettingsDetails.size() > 0)
            shipmentSettings = shipmentSettingsDetails.get(0);
        else {
            log.error("Not able to create consolidation, Shipment Settings not available in current tenant");
            return;
        }
        if(shipmentSettings.getShipConsolidationContainerEnabled()) {
            ConsolidationDetails consolidationDetails = new ConsolidationDetails();
            consolidationDetails.setConsolidationType(Constants.SHIPMENT_TYPE_DRT);
            consolidationDetails.setTransportMode(shipmentDetails.getTransportMode());
            if((shipmentSettings.getConsolidationLite() == null || !shipmentSettings.getConsolidationLite()) &&
                    (shipmentDetails.getCarrierDetails().getOriginPort() == null || shipmentDetails.getCarrierDetails().getOriginPort().isEmpty()
                    || shipmentDetails.getCarrierDetails().getDestinationPort() == null || shipmentDetails.getCarrierDetails().getDestinationPort().isEmpty())) {
                throw new ValidationException("Not able to create consolidation, before adding 'New Containers' , pleasenprovide ‘Origin’ and ‘Destination’ values.");
            }
            if(shipmentDetails.getCarrierDetails().getOriginPort() == shipmentDetails.getCarrierDetails().getDestinationPort()) {
                throw new ValidationException("‘Origin’ and ‘Destination’ can't be same");
            }
            consolidationDetails.setCarrierDetails(jsonHelper.convertValue(shipmentDetails.getCarrierDetails(), CarrierDetails.class));
            consolidationDetails.getCarrierDetails().setId(null);
            consolidationDetails.getCarrierDetails().setGuid(null);
            consolidationDetails.setShipmentType(shipmentDetails.getDirection());
            consolidationDetails.setContainerCategory(shipmentDetails.getShipmentType());
            consolidationDetails.setIsReceivingAgentFreeTextAddress(false);
            consolidationDetails.setIsSendingAgentFreeTextAddress(false);
            consolidationDetails.setIsInland(false);
            consolidationDetails.setSourceTenantId(TenantContext.getCurrentTenant().longValue());
            consolidationService.generateConsolidationNumber(consolidationDetails);
            // TODO- which one is CarrierBookingRef
            // TODO- default organizations Row -- setAgentOrganizationAndAddress() function in v1
//            if(consolidationDetails.getShipmentType() != null && !consolidationDetails.getShipmentType().isEmpty()
//            && consolidationDetails.getShipmentType().equals(Constants.IMP) || consolidationDetails.getShipmentType().equals(Constants.DIRECTION_EXP)) {
//                Parties defaultParty = new Parties(); // TODO- fetch from tenants row
//                if(true) { // TODO- add conditions on default Party
//                    if(consolidationDetails.getShipmentType().equals(Constants.DIRECTION_EXP)) {
//
//                    }
//                }
//            }
            consolidationDetails = consolidationDetailsDao.save(consolidationDetails, false);
            Long id = consolidationDetails.getId();
            if(containers != null && containers.size() > 0) {
                containers = containers.stream().map(e -> e.setConsolidationId(id)).collect(Collectors.toList());
                containers = containerDao.saveAll(containers);
            }
            consolidationDetails.setContainersList(containers);
            attachConsolidations(shipmentDetails.getId(), List.of(id));
            consolidationService.afterSave(consolidationDetails, true);
            try {
                consolidationSync.sync(consolidationDetails);
            } catch (Exception e) {
                log.error("Error performing sync on consol entity, {}", e);
            }
        }
    }

    @Override
    public void exportExcel(HttpServletResponse response, CommonRequestModel commonRequestModel) throws IOException, IllegalAccessException {
        String responseMsg;

        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Shipment list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        request.setIncludeTbls(Arrays.asList("additionalDetails", "client", "consigner", "consignee", "carrierDetails"));
        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
        log.info("Shipment list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet("ShipmentList");
        makeHeadersInSheet(sheet);

        //Filling the data
        List<IRunnerResponse> shipmentListResponseData = convertEntityListToDtoList(shipmentDetailsPage.getContent());

        for (int i = 0; i < shipmentListResponseData.size(); i++) {
            Row itemRow = sheet.createRow(i + 2);
            ShipmentListResponse shipment = (ShipmentListResponse) shipmentListResponseData.get(i);
            var shipmentBasicValues = parser.getAllAttributeValuesAsList(shipment);
            int offset = 0;
            for (int j = 0; j < shipmentBasicValues.size(); j++)
                itemRow.createCell(offset + j).setCellValue(shipmentBasicValues.get(j));
            offset += shipmentBasicValues.size();

            var shipmentClientValues = parser.getAllAttributeValuesAsListForParty(shipment.getClient());
            for (int j = 0; j < shipmentClientValues.size(); j++)
                itemRow.createCell(offset + j).setCellValue(shipmentClientValues.get(j));
            offset += shipmentClientValues.size();

            var shipmentConsigneeValues = parser.getAllAttributeValuesAsListForParty(shipment.getConsignee());
            for (int j = 0; j < shipmentConsigneeValues.size(); j++)
                itemRow.createCell(offset + j).setCellValue(shipmentConsigneeValues.get(j));
            offset += shipmentConsigneeValues.size();

            var shipmentConsignerValues = parser.getAllAttributeValuesAsListForParty(shipment.getConsignee());
            for (int j = 0; j < shipmentConsignerValues.size(); j++)
                itemRow.createCell(offset + j).setCellValue(shipmentConsignerValues.get(j));
            offset += shipmentConsignerValues.size();

            var carrierDetails = parser.getAllAttributeValuesAsListForCarrier(shipment.getCarrierDetails());
            for (int j = 0; j < carrierDetails.size(); j++)
                itemRow.createCell(offset + j).setCellValue(carrierDetails.get(j));
            offset += carrierDetails.size();

            var pickupDetails = parser.getAllAttributeValuesAsListForPDDetail(shipment.getPickupDetails());
            for (int j = 0; j < pickupDetails.size(); j++)
                itemRow.createCell(offset + j).setCellValue(pickupDetails.get(j));
            offset += pickupDetails.size();

            var deliveryDetails = parser.getAllAttributeValuesAsListForPDDetail(shipment.getDeliveryDetails());
            for (int j = 0; j < deliveryDetails.size(); j++)
                itemRow.createCell(offset + j).setCellValue(deliveryDetails.get(j));
            offset += deliveryDetails.size();
        }

        LocalDateTime currentTime = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
        String timestamp = currentTime.format(formatter);
        String filenameWithTimestamp = "Shipments_" + timestamp + ".xlsx";

        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-Disposition", "attachment; filename=" + filenameWithTimestamp);

        try (OutputStream outputStream = response.getOutputStream()) {
            workbook.write(outputStream);
        }

    }

    private void makeHeadersInSheet(Sheet sheet) {
        Row preHeaderRow = sheet.createRow(0);
        Row headerRow = sheet.createRow(1);
        List<String> shipmentHeader = parser.getHeadersForShipment();
        for (int i = 0; i < shipmentHeader.size(); i++) {
            Cell cell = headerRow.createCell(i);
            cell.setCellValue(shipmentHeader.get(i));
        }

        //Create Parties Headers
        int offSet = shipmentHeader.size();

        preHeaderRow.createCell(offSet).setCellValue("Client");
        List<String> partiesClientHeader = parser.getHeadersForParties();
        for (int i = 0; i < partiesClientHeader.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(partiesClientHeader.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + partiesClientHeader.size() - 1));
        offSet += partiesClientHeader.size();

        preHeaderRow.createCell(offSet).setCellValue("Consignee");
        List<String> partiesConsigneeHeader = parser.getHeadersForParties();
        for (int i = 0; i < partiesConsigneeHeader.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(partiesConsigneeHeader.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + partiesConsigneeHeader.size() - 1));
        offSet += partiesConsigneeHeader.size();

        preHeaderRow.createCell(offSet).setCellValue("Consigner");
        List<String> partiesConsignerHeader = parser.getHeadersForParties();
        for (int i = 0; i < partiesConsignerHeader.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(partiesConsignerHeader.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + partiesConsignerHeader.size() - 1));
        offSet += partiesConsignerHeader.size();

        preHeaderRow.createCell(offSet).setCellValue("Carrier Details");
        List<String> carrierHeader = parser.getHeadersForCarrier();
        for (int i = 0; i < carrierHeader.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(carrierHeader.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + carrierHeader.size() - 1));
        offSet += carrierHeader.size();

        preHeaderRow.createCell(offSet).setCellValue("Pickup Details");
        List<String> pickupDeliveryDetails = parser.getHeadersForPDDetails();
        for (int i = 0; i < pickupDeliveryDetails.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(pickupDeliveryDetails.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + pickupDeliveryDetails.size() - 1));
        offSet += pickupDeliveryDetails.size();

        preHeaderRow.createCell(offSet).setCellValue("Delivery Details");
        for (int i = 0; i < pickupDeliveryDetails.size(); i++) {
            Cell cell = headerRow.createCell(offSet + i);
            cell.setCellValue(pickupDeliveryDetails.get(i));
        }
        sheet.addMergedRegion(new CellRangeAddress(0, 0, offSet, offSet + pickupDeliveryDetails.size() - 1));
        offSet += pickupDeliveryDetails.size();

    }


    public ResponseEntity<?> fullShipmentsList(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            request.setIncludeTbls(Arrays.asList("additionalDetails", "client", "consigner", "consignee", "carrierDetails"));
//            checkWayBillNumberCriteria(request);
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Shipment list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(convertEntityListToFullShipmentList(shipmentDetailsPage.getContent()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> assignShipmentContainers(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ShipmentContainerAssignRequest request = (ShipmentContainerAssignRequest) commonRequestModel.getData();
            shipmentsContainersMappingDao.assignContainers(request.getShipmentId(), request.getContainerIds());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
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

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            request.setIncludeTbls(Arrays.asList("additionalDetails", "client", "consigner", "consignee", "carrierDetails"));
            checkWayBillNumberCriteria(request);
            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Shipment list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
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
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment async list for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            request.setIncludeTbls(Arrays.asList("additionalDetails", "client", "consigner", "consignee", "carrierDetails"));
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

    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
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
                log.debug("Shipment Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
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

    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Shipment details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ShipmentDetailsResponse response = modelMapper.map(shipmentDetails.get(), ShipmentDetailsResponse.class);
            createShipmentPayload(shipmentDetails.get(), response);
            //containerCountUpdate(shipmentDetails.get(), response);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Async
    public CompletableFuture<ResponseEntity<?>> retrieveByIdAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Shipment async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            shipmentDetails.get().setFileRepoList(fileRepoDao.findByEntityIdAndEntityType(id, Constants.SHIPMENT));
            shipmentDetails.get().setNotesList(notesDao.findByEntityIdAndEntityType(id, Constants.SHIPMENT));
            log.info("Shipment details async fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ShipmentDetailsResponse response = jsonHelper.convertValue(shipmentDetails.get(), ShipmentDetailsResponse.class);
            //containerCountUpdate(shipmentDetails.get(), response);
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(response));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    public ResponseEntity<?> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException {
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment complete retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Shipment complete retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            CommonRequestModel commonListRequestModel = CommonRequestModel.buildRequest(constructListCommonRequest("shipmentId", id, "="));
            CommonRequestModel commonListRequestModelbyEntityId = CommonRequestModel.buildRequest(constructListCommonRequest("entityId", id, "="));

            CompletableFuture<ResponseEntity<?>> shipmentsFuture = retrieveByIdAsync(commonRequestModel);
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
    public ResponseEntity<?> partialUpdate(CommonRequestModel commonRequestModel) throws Exception {

        ShipmentPatchRequest shipmentRequest = (ShipmentPatchRequest) commonRequestModel.getData();
        if ((shipmentRequest.getId() == null && shipmentRequest.getGuid() == null) && (shipmentRequest.getShipmentId() == null || shipmentRequest.getShipmentId().get() == "")) {
            log.error("Request Id is null for update request with Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new Exception("Request Id is null");
        }
        List<BookingCarriageRequest> bookingCarriageRequestList = shipmentRequest.getBookingCarriagesList();
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
        CarrierDetailRequest carrierDetailRequest = shipmentRequest.getCarrierDetails();
        // TODO- implement Validation logic
        Long id = null;
        Optional<ShipmentDetails> oldEntity = null;
        ShipmentRequest fetchShipmentRequest = new ShipmentRequest();
        fetchShipmentRequest.setId(shipmentRequest.getId() != null ? shipmentRequest.getId().get() : null);
        fetchShipmentRequest.setGuid(shipmentRequest.getGuid());
        if(shipmentRequest.getId() != null || shipmentRequest.getGuid() != null) {
            oldEntity = retrieveByIdOrGuid(fetchShipmentRequest);
            id = oldEntity.get().getId();
        }
        else {
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentRequest.getShipmentId().get(), "=");
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
            log.debug("Shipment Details is null for Id {}", shipmentRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try {
            ShipmentDetails entity = oldEntity.get();
            shipmentDetailsMapper.update(shipmentRequest, entity);
            entity.setId(oldEntity.get().getId());
            List<Containers> updatedContainers = null;
            if (containerRequestList != null) {
                updatedContainers = containerDao.updateEntityFromShipmentConsole(convertToEntityList(containerRequestList, Containers.class), null);
            } else {
                updatedContainers = oldEntity.get().getContainersList();
            }
            entity.setContainersList(updatedContainers);
            AdditionalDetails updatedAdditionalDetails = null;
            if (additionalDetailRequest != null) {
                updatedAdditionalDetails = additionalDetailDao.updateEntityFromShipment(convertToClass(additionalDetailRequest, AdditionalDetails.class));
                entity.setAdditionalDetails(updatedAdditionalDetails);
            }
            CarrierDetails updatedCarrierDetails = null;
            if (carrierDetailRequest != null) {
                updatedCarrierDetails = carrierDao.updateEntityFromShipmentConsole(convertToClass(carrierDetailRequest, CarrierDetails.class));
                entity.setCarrierDetails(updatedCarrierDetails);
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
                List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(convertToEntityList(bookingCarriageRequestList, BookingCarriage.class), id);
                entity.setBookingCarriagesList(updatedBookingCarriages);
            }
            if (packingRequestList != null) {
                List<Packing> updatedPackings = packingDao.updateEntityFromShipment(convertToEntityList(packingRequestList, Packing.class), id);
                entity.setPackingList(updatedPackings);
            }
            if (elDetailsRequestList != null) {
                List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(convertToEntityList(elDetailsRequestList, ELDetails.class), id);
                entity.setElDetailsList(updatedELDetails);
            }
            if (eventsRequestList != null) {
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(convertToEntityList(eventsRequestList, Events.class), id, Constants.SHIPMENT);
                entity.setEventsList(updatedEvents);
            }
            if (fileRepoRequestList != null) {
                List<FileRepo> updatedFileRepos = fileRepoDao.updateEntityFromOtherEntity(convertToEntityList(fileRepoRequestList, FileRepo.class), id, Constants.SHIPMENT);
                entity.setFileRepoList(updatedFileRepos);
            }
            if (jobRequestList != null) {
                List<Jobs> updatedJobs = jobDao.updateEntityFromShipment(convertToEntityList(jobRequestList, Jobs.class), id);
                entity.setJobsList(updatedJobs);
            }
            if (notesRequestList != null) {
                List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(convertToEntityList(notesRequestList, Notes.class), id, Constants.SHIPMENT);
                entity.setNotesList(updatedNotes);
            }
            if (referenceNumbersRequestList != null) {
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id);
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (routingsRequestList != null) {
                List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(convertToEntityList(routingsRequestList, Routings.class), id);
                entity.setRoutingsList(updatedRoutings);
            }
            if (serviceDetailsRequestList != null) {
                List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(convertToEntityList(serviceDetailsRequestList, ServiceDetails.class), id);
                entity.setServicesList(updatedServiceDetails);
            }

            try {
                shipmentSync.sync(entity);
            } catch (Exception e) {
                log.error("Error performing sync on shipment entity, {}", e);
            }

            afterSave(entity, false);
            ShipmentDetailsResponse response = shipmentDetailsMapper.map(entity);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> toggleLock(CommonRequestModel commonRequestModel) {
        CommonGetRequest commonGetRequest = (CommonGetRequest) commonRequestModel.getData();
        Long id = commonGetRequest.getId();
        ShipmentDetails shipmentDetails = shipmentDao.findById(id).get();
        String lockingUser = shipmentDetails.getLockedBy();
        String currentUser = userContext.getUser().getUsername();

        if (shipmentDetails.getIsLocked() != null && shipmentDetails.getIsLocked()) {
            if (lockingUser != null && lockingUser.equals(currentUser))
                shipmentDetails.setIsLocked(false);
        } else {
            shipmentDetails.setIsLocked(true);
            shipmentDetails.setLockedBy(currentUser);
        }
        shipmentDetails = shipmentDao.save(shipmentDetails, false);
        afterSave(shipmentDetails, false);
        return ResponseHelper.buildSuccessResponse();
    }


    private <T extends IRunnerResponse> List<T> getResponse(CompletableFuture<ResponseEntity<?>> responseEntity) throws ExecutionException, InterruptedException {
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

    private String generateShipmentId() {
        List<ShipmentSettingsDetails> shipmentSettingsList = shipmentSettingsDao.list();
        String shipmentId = "";
        boolean flag = true;

        while(flag) {
            ListCommonRequest listRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listRequest, ShipmentDetails.class);
            Page<ShipmentDetails> shipments = shipmentDao.findAll(pair.getLeft(), pair.getRight());

            if(!shipmentId.isEmpty() && shipments.getTotalElements() == 0)
                flag = false;
            else {
                if(shipmentSettingsList.get(0) != null && shipmentSettingsList.get(0).getCustomisedSequence()) {
                    try{
                        shipmentId = getCustomizedShipmentProcessNumber(shipmentSettingsList.get(0), ProductProcessTypes.ShipmentNumber);
                    } catch (Exception ignored) {
                        //
                        shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
                    }
                } else {
                    shipmentId = Constants.SHIPMENT_ID_PREFIX + getShipmentsSerialNumber();
                }
            }
        }
        return shipmentId;
//        if (shipmentSettingsList.isEmpty())
//            return StringUtility.getRandomString(10);
//        return createShipmentSequence(shipmentSettingsList.get(0));
    }

    private String getCustomizedShipmentProcessNumber(ShipmentSettingsDetails shipmentSettingsDetails, ProductProcessTypes productProcessType) {
        var productEngine = new ProductIdentifierUtility();
        productEngine.populateEnabledTenantProducts(shipmentSettingsDetails);
        // to check the commmon sequence
        var sequenceNumber = productEngine.GetCommonSequenceNumber(currentShipment.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI);
        if (sequenceNumber != null && !sequenceNumber.isEmpty()) {
            return sequenceNumber;
        }
        var identifiedProduct = productEngine.IdentifyProduct(currentShipment);
        if (identifiedProduct == null){
            return "";
        }
        var sequenceSettings = getNextNumberHelper.getProductSequence(identifiedProduct.getId(), productProcessType);
        if(sequenceSettings == null){
            sequenceSettings = productEngine.getShipmentProductWithOutContainerType(currentShipment, productProcessType);
            if (sequenceSettings == null)
            {
                // get default product type for shipment
                var defaultProduct = productEngine.getDefaultShipmentProduct();
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
//        SequenceIncrementor sequenceIncrementor = SequenceIncrementor.builder().entityId(1L).build();
//        sequenceIncrementorDao.save(sequenceIncrementor);
//        return sequenceIncrementor.getShipmentIncrementId().toString();
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

    @Transactional
    public ResponseEntity<?> completeV1ShipmentCreateAndUpdate(CommonRequestModel commonRequestModel, Map<UUID, String> map) throws Exception {

        ShipmentRequest shipmentRequest = (ShipmentRequest) commonRequestModel.getData();

        List<BookingCarriageRequest> bookingCarriageRequestList = shipmentRequest.getBookingCarriagesList();
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
        CarrierDetailRequest carrierDetailRequest = shipmentRequest.getCarrierDetails();

        // TODO- implement Validation logic
        UUID guid = shipmentRequest.getGuid();
        Optional<ShipmentDetails> oldEntity = shipmentDao.findByGuid(guid);

        List<Long> tempConsolIds = new ArrayList<>();

        List<ConsolidationDetailsRequest> consolidationDetailsRequests = shipmentRequest.getConsolidationList();
        if(consolidationDetailsRequests != null && !consolidationDetailsRequests.isEmpty()) {
            for(ConsolidationDetailsRequest consolidation : consolidationDetailsRequests) {
                Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findByGuid(consolidation.getGuid());
                if(consolidationDetails.get() != null && consolidationDetails.get().getId() != null) {
                    tempConsolIds.add(consolidationDetails.get().getId());
                }
            }
        }

        try {
            List<Containers> oldContainers = null;
            Long id = null;
            ShipmentDetails oldShipment = null;
            boolean isCreate = true;
            if(oldEntity != null && oldEntity.isPresent()) {
                oldShipment = oldEntity.get();
                id = oldEntity.get().getId();
                oldContainers = oldEntity.get().getContainersList();
                isCreate = false;
            }
            ShipmentDetails entity = objectMapper.convertValue(shipmentRequest, ShipmentDetails.class);
            entity.setId(id);
            List<Containers> updatedContainers = null;
            if (containerRequestList != null) {
                updatedContainers = containerDao.updateEntityFromShipmentV1(convertToEntityList(containerRequestList, Containers.class), oldContainers);
            } else if(oldEntity != null && !oldEntity.isEmpty()){
                updatedContainers = oldEntity.get().getContainersList();
            }
            entity.setContainersList(updatedContainers);

            if(id == null) {
                entity = shipmentDao.save(entity, true);
                id = entity.getId();
            } else {
                entity = shipmentDao.update(entity, true);
            }

            attachConsolidations(entity.getId(), tempConsolIds);

            if (bookingCarriageRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<BookingCarriage>, Pageable> bookingCarriagePair = fetchData(listCommonRequest, BookingCarriage.class);
                Page<BookingCarriage> oldBookingCarriages = bookingCarriageDao.findAll(bookingCarriagePair.getLeft(), bookingCarriagePair.getRight());
                List<BookingCarriage> updatedBookingCarriages = bookingCarriageDao.updateEntityFromShipment(convertToEntityList(bookingCarriageRequestList, BookingCarriage.class), id, oldBookingCarriages.stream().toList());
                entity.setBookingCarriagesList(updatedBookingCarriages);
            }
            if (packingRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<Packing>, Pageable> packingPair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> oldPackings = packingDao.findAll(packingPair.getLeft(), packingPair.getRight());
                List<Packing> updatedPackings = packingDao.updateEntityFromShipment(convertToEntityList(packingRequestList, Packing.class), id, oldPackings.stream().toList(), updatedContainers, map);
                entity.setPackingList(updatedPackings);
            }
            if (elDetailsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<ELDetails>, Pageable> elDetailsPair = fetchData(listCommonRequest, ELDetails.class);
                Page<ELDetails> oldELDetails = elDetailsDao.findAll(elDetailsPair.getLeft(), elDetailsPair.getRight());
                List<ELDetails> updatedELDetails = elDetailsDao.updateEntityFromShipment(convertToEntityList(elDetailsRequestList, ELDetails.class), id, oldELDetails.stream().toList());
                entity.setElDetailsList(updatedELDetails);
            }
            if (eventsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT);
                Pair<Specification<Events>, Pageable> pair = fetchData(listCommonRequest, Events.class);
                Page<Events> oldEvents = eventDao.findAll(pair.getLeft(), pair.getRight());
                List<Events> updatedEvents = eventDao.updateEntityFromOtherEntity(convertToEntityList(eventsRequestList, Events.class), id, Constants.SHIPMENT, oldEvents.stream().toList());
                entity.setEventsList(updatedEvents);
            }
            if (jobRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<Jobs>, Pageable> pair = fetchData(listCommonRequest, Jobs.class);
                Page<Jobs> oldJobs = jobDao.findAll(pair.getLeft(), pair.getRight());
                List<Jobs> updatedJobs = jobDao.updateEntityFromShipment(convertToEntityList(jobRequestList, Jobs.class), id, oldJobs.stream().toList());
                entity.setJobsList(updatedJobs);
            }
            if (referenceNumbersRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<ReferenceNumbers>, Pageable> pair = fetchData(listCommonRequest, ReferenceNumbers.class);
                Page<ReferenceNumbers> oldReferenceNumbers = referenceNumbersDao.findAll(pair.getLeft(), pair.getRight());
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id, oldReferenceNumbers.stream().toList());
                entity.setReferenceNumbersList(updatedReferenceNumbers);
            }
            if (routingsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
                Page<Routings> oldRoutings = routingsDao.findAll(pair.getLeft(), pair.getRight());
                List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(convertToEntityList(routingsRequestList, Routings.class), id, oldRoutings.stream().toList());
                entity.setRoutingsList(updatedRoutings);
            }
            if (serviceDetailsRequestList != null) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", entity.getId(), "=");
                Pair<Specification<ServiceDetails>, Pageable> pair = fetchData(listCommonRequest, ServiceDetails.class);
                Page<ServiceDetails> oldServiceDetails = serviceDetailsDao.findAll(pair.getLeft(), pair.getRight());
                List<ServiceDetails> updatedServiceDetails = serviceDetailsDao.updateEntityFromShipment(convertToEntityList(serviceDetailsRequestList, ServiceDetails.class), id, oldServiceDetails.stream().toList());
                entity.setServicesList(updatedServiceDetails);
            }
            if (fileRepoRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT);
                Pair<Specification<FileRepo>, Pageable> pair = fetchData(listCommonRequest, FileRepo.class);
                Page<FileRepo> oldFileRepoList = fileRepoDao.findAll(pair.getLeft(), pair.getRight());
                List<FileRepo> updatedFileRepos = fileRepoDao.updateEntityFromOtherEntity(convertToEntityList(fileRepoRequestList, FileRepo.class), id, Constants.SHIPMENT, oldFileRepoList.stream().toList());
                entity.setFileRepoList(updatedFileRepos);
            }
            if (notesRequestList != null) {
                ListCommonRequest listCommonRequest = constructListRequestFromEntityId(entity.getId(), Constants.SHIPMENT);
                Pair<Specification<Notes>, Pageable> pair = fetchData(listCommonRequest, Notes.class);
                Page<Notes> oldNoteList = notesDao.findAll(pair.getLeft(), pair.getRight());
                List<Notes> updatedNotes = notesDao.updateEntityFromOtherEntity(convertToEntityList(notesRequestList, Notes.class), id, Constants.SHIPMENT, oldNoteList.stream().toList());
                entity.setNotesList(updatedNotes);
            }
            afterSave(entity, isCreate);
            ShipmentDetailsResponse response = shipmentDetailsMapper.map(entity);

            return ResponseHelper.buildSuccessResponse(response);
        } catch (ExecutionException e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }


    private void createShipmentPayload (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        try {
            this.addAllMasterDataInSingleCall(shipmentDetails, shipmentDetailsResponse);
            this.addAllUnlocationDataInSingleCall(shipmentDetails, shipmentDetailsResponse);
            this.addAllCarrierDataInSingleCall(shipmentDetails, shipmentDetailsResponse);
            this.addAllCurrencyDataInSingleCall(shipmentDetails, shipmentDetailsResponse);
            this.addAllCommodityTypesInSingleCall(shipmentDetails, shipmentDetailsResponse);
            this.addAllTenantDataInSingleCall(shipmentDetails, shipmentDetailsResponse);
            this.addAllWarehouseDataInSingleCall(shipmentDetails, shipmentDetailsResponse);
        }
        catch (Exception ex) {
            log.error("Request: {} || Error occured for event: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_RETRIEVE, ex.getLocalizedMessage());
        }

    }
    private void addAllMasterDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {

        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<MasterListRequest> listRequests = new ArrayList<>(masterDataUtils.createInBulkMasterListRequest(shipmentDetailsResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName() ));
        if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName() ));
        if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
            listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() ));

        Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(listRequests);
        masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST);

        shipmentDetailsResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName()), CacheConstants.MASTER_LIST));
        if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            shipmentDetailsResponse.getAdditionalDetails().setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.MASTER_LIST) );
        if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
                shipmentDetailsResponse.getCarrierDetails().setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.MASTER_LIST) );

    }



    private void addAllUnlocationDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> locationCodes = new ArrayList<>();
        if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
            locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName() )));
        if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName() )));
        // TODO: This needs to be change to fetch based on LocationServiceGuid once UI is ready
        Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.UNLOCATION_CODE);
        masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS);

        if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails()))
            shipmentDetailsResponse.getCarrierDetails().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS));
        if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            shipmentDetailsResponse.getAdditionalDetails().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS));
    }

    private void addAllTenantDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> tenantIdList = new ArrayList<>(masterDataUtils.createInBulkTenantsRequest(shipmentDetailsResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName()));
        Map v1Data = masterDataUtils.fetchInTenantsList(tenantIdList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.TENANTS);
        shipmentDetailsResponse.setTenantIdsData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName()), CacheConstants.TENANTS));
    }

    private void addAllCurrencyDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        List<String> currencyList = new ArrayList<>(masterDataUtils.createInBulkCurrencyRequest(shipmentDetailsResponse, ShipmentDetails.class, fieldNameKeyMap, ShipmentDetails.class.getSimpleName()));
        Map v1Data = masterDataUtils.fetchInCurrencyList(currencyList);
        masterDataUtils.pushToCache(v1Data, CacheConstants.CURRENCIES);
        shipmentDetailsResponse.setCurrenciesMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(ShipmentDetails.class.getSimpleName()), CacheConstants.CURRENCIES));
    }

    private void addAllCarrierDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        if (!Objects.isNull(shipmentDetailsResponse.getCarrierDetails())) {
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            List<String> carrierList = new ArrayList<>(masterDataUtils.createInBulkCarriersRequest(shipmentDetailsResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName()));
            Map v1Data = masterDataUtils.fetchInBulkCarriers(carrierList);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER);
            shipmentDetailsResponse.getCarrierDetails().setCarrierMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.CARRIER));
        }
    }

    private void addAllCommodityTypesInSingleCall(ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> containerTypes = new HashSet<>();
        if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
            shipmentDetailsResponse.getContainersList().forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId() )));

        Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(containerTypes.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY);

        if (!Objects.isNull(shipmentDetailsResponse.getContainersList()))
            shipmentDetailsResponse.getContainersList().forEach(r -> r.setCommodityTypeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.COMMODITY)));
    }

    private void addAllWarehouseDataInSingleCall (ShipmentDetails shipmentDetails, ShipmentDetailsResponse shipmentDetailsResponse) {
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        Set<String> wareHouseTypes = new HashSet<>();
        if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            wareHouseTypes.addAll(masterDataUtils.createInBulkWareHouseRequest(shipmentDetailsResponse.getAdditionalDetails(), AdditionalDetails.class, fieldNameKeyMap, AdditionalDetails.class.getSimpleName()) );

        Map v1Data = masterDataUtils.fetchInWareHousesList(wareHouseTypes.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.WAREHOUSES);

        if (!Objects.isNull(shipmentDetailsResponse.getAdditionalDetails()))
            shipmentDetailsResponse.getAdditionalDetails().setTextData(masterDataUtils.setMasterData(fieldNameKeyMap.get(AdditionalDetails.class.getSimpleName()), CacheConstants.WAREHOUSES));

    }

    public ResponseEntity<?> cloneShipment(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            ShipmentRequest cloneShipmentDetails = jsonHelper.convertValue(shipmentDetails.get(), ShipmentRequest.class);
            cloneShipmentDetails.setHouseBill(null);
            cloneShipmentDetails.setBookingReference(null);
            cloneShipmentDetails.setContainersList(null);
            cloneShipmentDetails.setRoutingsList(null);
            cloneShipmentDetails.setShipmentId(null);
            cloneShipmentDetails.setMasterBill(null);
            cloneShipmentDetails.setConsolidationList(null);
            
            if(Constants.TRANSPORT_MODE_SEA.equals(cloneShipmentDetails.getTransportMode()) && Constants.DIRECTION_EXP.equals(cloneShipmentDetails.getDirection()))
                cloneShipmentDetails.setHouseBill(generateCustomHouseBL());

            CommonRequestModel requestModel = CommonRequestModel.buildRequest(cloneShipmentDetails);
            log.info("Shipment details cloning started for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return this.create(requestModel);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> transportInstructionList(CommonRequestModel commonRequestModel) {
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
    public ResponseEntity<?> containerListForTI(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            TIListRequest tiListRequest = (TIListRequest) commonRequestModel.getData();
            if(tiListRequest == null) {
                log.error("Request is empty for container TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(tiListRequest.getShipmentGuid() == null) {
                log.error("Shipment Guid is null for conatiner TI List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            V1DataResponse v1DataResponse = v1Service.fetchContainersListForTI(tiListRequest);
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
    public ResponseEntity<?> retrieveByOrderId(String orderId) {
        try {
            ShipmentDetailsResponse response = jsonHelper.convertValue(orderManagementAdapter.getOrder(orderId), ShipmentDetailsResponse.class);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e){
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<?> generateCustomHouseBLNumber() {
        try {
            return ResponseHelper.buildSuccessResponse(generateCustomHouseBL());
        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<?> getConsolFromShipment(Long shipmentId) {
        ConsolidationDetailsResponse consol;
        var shipmentRes = shipmentDao.findById(shipmentId);

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

        consol = ConsolidationDetailsResponse.builder()
                .consolidationType(Constants.SHIPMENT_TYPE_DRT)
                .transportMode(shipment.getTransportMode())
                .containerCategory(shipment.getShipmentType())
                .declarationType(additionalDetails != null ? additionalDetails.getCustomDeclType() : null)
                .carrierDetails(CarrierDetailResponse.builder()
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
                .shipmentType(shipment.getShipmentType())
                .igmFileDate(additionalDetails != null ? additionalDetails.getIGMFileDate() : null)
                .igmFileNo(additionalDetails != null ? additionalDetails.getIGMFileNo() : null)
                .smtpigmDate(additionalDetails != null ? additionalDetails.getSMTPIGMDate() : null)
                .smtpigmNumber(additionalDetails != null ? additionalDetails.getSMTPIGMNumber() : null)
                .igmInwardDate(additionalDetails != null ? additionalDetails.getIGMInwardDate() : null)
                .inwardDateAndTime(additionalDetails != null ? additionalDetails.getInwardDateAndTime() : null)
                .warehouseId(additionalDetails != null ? additionalDetails.getWarehouseId() : null)
                .bol(shipment.getMasterBill())
                .referenceNumber(shipment.getBookingReference())
                .payment(isPayment ? shipment.getPaymentTerms() : null)
                .routingsList(List.of(customRouting))
                .mawb(isMawb ? shipment.getMasterBill() : null)
                .isLinked(true)
                .build();

        return ResponseHelper.buildSuccessResponse(consol);
    }

    private String generateCustomHouseBL() {
        String res = null;
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
        ShipmentSettingsDetails tenantSetting = null;
        if (shipmentSettingsDetailsList.get(0) != null)
            tenantSetting = shipmentSettingsDetailsList.get(0);

        if (tenantSetting.getRestrictHblGen() && tenantSetting.getCustomisedSequence()) {
            // generate via Product Identifier Utility
            // res = someMethod();
        }

        if(res == null || res.isEmpty()) {
            res = tenantSetting.getHousebillPrefix();
            switch(tenantSetting.getHousebillNumberGeneration()) {
                case "Random" :
                    res += StringUtility.getRandomString(10);
                    break;
                case "Serial" :
                    String serialNumber = v1Service.getMaxShipmentId();
//                    Long serialNumber = shipmentDao.findMaxId() + 1;
                    res += serialNumber;
                    break;
                default : res = "";
                    break;
            }
        }

        return res;
    }

    @Override
    public ResponseEntity<?> getDefaultShipment() {
        String responseMsg;
        try {
            List<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
            if(shipmentSettingsDetails == null || shipmentSettingsDetails.size() == 0)
                throw new RunnerException("Shipment settings empty for current tenant");
            var tenantSettings = shipmentSettingsDetails.get(0);
            // Populate shipment details on basis of tenant settings
            ShipmentDetailsResponse response = new ShipmentDetailsResponse();
            response.setAdditionalDetails(new AdditionalDetailResponse());
            response.setCarrierDetails(new CarrierDetailResponse());
            response.setTransportMode(tenantSettings.getDefaultTransportMode());
            response.setDirection(tenantSettings.getDefaultShipmentType());
            response.setShipmentType(tenantSettings.getDefaultContainerType());
            this.addAllMasterDataInSingleCall(null, response);

            return ResponseHelper.buildSuccessResponse(response);
        } catch(Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

}
