package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.*;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.npm.*;
import com.dpw.runner.shipment.services.dto.request.platformBooking.PlatformToRunnerCustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.request.ApprovalPartiesRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CreateShipmentTaskFromBookingTaskRequest;
import com.dpw.runner.shipment.services.dto.v1.request.ShipmentBillingListRequest;
import com.dpw.runner.shipment.services.dto.v1.request.V1RetrieveRequest;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingSource;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.entity.enums.PartyType;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.dto.OrderManageDto;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import com.dpw.runner.shipment.services.service.interfaces.IQuoteContractsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.modelmapper.ModelMapper;
import org.slf4j.MDC;
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

import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;
import static com.dpw.runner.shipment.services.validator.constants.CustomerBookingConstants.*;

@Service
@Slf4j
public class CustomerBookingService implements ICustomerBookingService {
    private static final Random rnd = new SecureRandom();
    private static final Map<String, String> loadTypeMap = Map.of("SEA", "LCL", "AIR", "LSE");
    ExecutorService executorService = Executors.newFixedThreadPool(10);
    @Autowired
    UserContext userContext;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private IMDMServiceAdapter mdmServiceAdapter;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private ICustomerBookingDao customerBookingDao;
    @Autowired
    private IBookingChargesDao bookingChargesDao;
    @Autowired
    private IPackingDao packingDao;
    @Autowired
    private IRoutingsDao routingsDao;
    @Autowired
    private IContainerDao containerDao;
    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private IFusionServiceAdapter fusionServiceAdapter;
    @Autowired
    private INPMServiceAdapter npmService;
    @Autowired
    private BookingIntegrationsUtility bookingIntegrationsUtility;
    @Autowired
    private ICRPServiceAdapter crpServiceAdapter;
    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private BillingServiceUrlConfig billingServiceUrlConfig;
    @Autowired
    private BillingServiceAdapter billingServiceAdapter;
    @Autowired
    private MasterDataFactory masterDataFactory;
    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private IOrderManagementAdapter orderManagementAdapter;
    @Autowired
    private KafkaProducer producer;
    @Autowired
    private IQuoteContractsService quoteContractsService;
    @Value("${booking.event.kafka.queue}")
    private String senderQueue;
    private final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("customerOrgCode", RunnerEntityMapping.builder().tableName("customer").dataType(String.class).fieldName(Constants.ORG_CODE).build()),
            Map.entry("consignerOrgCode", RunnerEntityMapping.builder().tableName("consignor").dataType(String.class).fieldName(Constants.ORG_CODE).build()),
            Map.entry("consigneeOrgCode", RunnerEntityMapping.builder().tableName("consignee").dataType(String.class).fieldName(Constants.ORG_CODE).build()),
            Map.entry("origin", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("origin").build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destination").build()),
            Map.entry("originPort", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("originPort").build()),
            Map.entry("destinationPort", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).dataType(String.class).fieldName("destinationPort").build()),
            Map.entry("bookingNumber", RunnerEntityMapping.builder().tableName(Constants.CUSTOMER_BOOKING).dataType(String.class).fieldName("bookingNumber").isContainsText(true).build()),
            Map.entry("bookingDate", RunnerEntityMapping.builder().tableName(Constants.CUSTOMER_BOOKING).dataType(LocalDateTime.class).fieldName("bookingDate").build()),
            Map.entry("bookingStatus", RunnerEntityMapping.builder().tableName(Constants.CUSTOMER_BOOKING).dataType(BookingStatus.class).fieldName("bookingStatus").build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName(Constants.CUSTOMER_BOOKING).dataType(String.class).fieldName("createdBy").build()),
            Map.entry("contractId", RunnerEntityMapping.builder().tableName(Constants.CUSTOMER_BOOKING).dataType(String.class).fieldName("contractId").build()),
            Map.entry("shipmentCreatedDate", RunnerEntityMapping.builder().tableName(Constants.CUSTOMER_BOOKING).dataType(LocalDateTime.class).fieldName("shipmentCreatedDate").build())
    );

    public static String getRandomNumberString(int digit) {
        int number = 0;
        if (digit == 7)
            number = rnd.nextInt(9999999);
        else
            number = rnd.nextInt(999999);
        if (digit == 7)
            return String.format("%07d", number);
        return String.format("%06d", number);
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) throws RunnerException {

        CustomerBookingRequest request = (CustomerBookingRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Customer Booking Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }

        CustomerBooking customerBooking = jsonHelper.convertValue(request, CustomerBooking.class);
        customerBooking.setSource(BookingSource.Runner);
        // Update NPM for contract utilization
        if (checkNPMContractUtilization(customerBooking)) {
            _npmContractUpdate(customerBooking, null, false, CustomerBookingConstants.REMOVE, false);
        }
        try {
            createEntities(customerBooking, request);
            /**
             * Platform service integration
             * Criteria for update call to platform service : check flag IsPlatformBookingCreated, if true then update otherwise dont update
             */
            //Check 1
            V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
            if (Objects.equals(customerBooking.getBookingStatus(), BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                    && (Boolean.FALSE.equals(v1TenantSettingsResponse.getFetchRatesMandate()) || (!Objects.isNull(customerBooking.getBookingCharges()) && !customerBooking.getBookingCharges().isEmpty()))) {
                CompletableFuture.runAsync(masterDataUtils.withMdc(() -> bookingIntegrationsUtility.createBookingInPlatform(customerBooking)), executorService);
            }
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RunnerException(e.getMessage());
        }
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(customerBooking, CustomerBookingResponse.class));
    }

    private void createEntities(CustomerBooking customerBooking, CustomerBookingRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        if (customerBooking.getIsPlatformBookingCreated() == null) {
            customerBooking.setIsPlatformBookingCreated(false);
        }
        if (customerBooking.getBookingNumber() == null) {
            customerBooking.setBookingNumber(generateBookingNumber(customerBooking.getCargoType()));
        }
        if (request.getOrderManagementId() != null) {
            Optional<CustomerBooking> booking = customerBookingDao.findByOrderManagementId(request.getOrderManagementId());
            if (booking.isPresent()) {
                CustomerBooking c = booking.get();
                c.setOrderManagementId(null);
                c.setOrderManagementNumber(null);
                customerBookingDao.save(c);
            }
        }
        populateTotalRevenueDetails(customerBooking, request);
        customerBooking = customerBookingDao.save(customerBooking);
        Long bookingId = customerBooking.getId();

        List<PackingRequest> packingRequest = request.getPackingList();
        if (packingRequest != null)
            customerBooking.setPackingList(packingDao.saveEntityFromBooking(commonUtils.convertToEntityList(packingRequest, Packing.class), bookingId));

        List<RoutingsRequest> routingsRequest = request.getRoutingList();
        if (routingsRequest != null)
            customerBooking.setRoutingList(routingsDao.saveEntityFromBooking(commonUtils.convertToEntityList(routingsRequest, Routings.class), bookingId));

        List<ContainerRequest> containerRequest = request.getContainersList();
        if (containerRequest != null) {
            List<Containers> containers = containerDao.updateEntityFromBooking(commonUtils.convertToEntityList(containerRequest, Containers.class), bookingId);
            customerBooking.setContainersList(containers);
        }

        List<Containers> containers = customerBooking.getContainersList();
        Map<UUID, Containers> containerMap = new HashMap<>();
        if (containers != null && !containers.isEmpty()) {
            for (Containers container : containers) {
                containerMap.put(container.getGuid(), container);
            }
        }
        List<BookingChargesRequest> bookingChargesRequest = request.getBookingCharges();
        if (bookingChargesRequest != null && !bookingChargesRequest.isEmpty()) {
            List<BookingCharges> bookingCharges = new ArrayList<>();
            for (BookingChargesRequest bookingChargeRequest : bookingChargesRequest) {
                List<UUID> containerUUIDs = bookingChargeRequest.getContainersUUID();
                BookingCharges bookingCharge = jsonHelper.convertValue(bookingChargeRequest, BookingCharges.class);
                List<Containers> containerList = new ArrayList<>();
                if (containerUUIDs != null && !containerUUIDs.isEmpty()) {
                    for (UUID uuid : containerUUIDs) {
                        Containers container = containerMap.get(uuid);
                        containerList.add(container);
                    }
                }
                bookingCharge.setContainersList(containerList);
                bookingCharge.setBookingId(customerBooking.getId());
                bookingCharges.add(bookingCharge);
            }
            bookingCharges = bookingChargesDao.updateEntityFromBooking(bookingCharges, customerBooking.getId());
            customerBooking.setBookingCharges(bookingCharges);
        }
        if (request.getOrderManagementId() != null) {
            pushCustomerBookingDataToDependentService(customerBooking, true);
        }
        try {
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(customerBooking)
                            .prevData(null)
                            .parent(CustomerBooking.class.getSimpleName())
                            .parentId(customerBooking.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );
        } catch (Exception e) {
            log.error(e.getMessage());
        }
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> cancel(CommonRequestModel commonRequestModel) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        return this.update(commonRequestModel);
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        CustomerBookingRequest request = (CustomerBookingRequest) commonRequestModel.getData();
        if (request == null || request.getId() == null) {
            log.error("Request is empty for Booking update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }
        long id = request.getId();
        Optional<CustomerBooking> oldEntity = customerBookingDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug(CustomerBookingConstants.BOOKING_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        if (Objects.equals(oldEntity.get().getBookingStatus(), BookingStatus.READY_FOR_SHIPMENT)) {
            throw new ValidationException("Booking alterations are not allowed once booking moved to Ready For Shipment.");
        }
        boolean isCreatedInPlatform = !Objects.isNull(oldEntity.get().getIsPlatformBookingCreated()) && oldEntity.get().getIsPlatformBookingCreated();
        CustomerBooking customerBooking = jsonHelper.convertValue(request, CustomerBooking.class);
        customerBooking.setCreatedAt(oldEntity.get().getCreatedAt());
        customerBooking.setCreatedBy(oldEntity.get().getCreatedBy());
        customerBooking.setIsPlatformBookingCreated(isCreatedInPlatform);
        customerBooking.setSource(oldEntity.get().getSource());

        // NPM update contract
        if (checkNPMContractUtilization(customerBooking)) {
            contractUtilisationForUpdate(customerBooking, oldEntity.get());
        }
        customerBooking = this.updateEntities(customerBooking, request, jsonHelper.convertToJson(oldEntity.get()));
        try {
            //Check 2
            V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
            if (!Objects.equals(customerBooking.getBookingStatus(), BookingStatus.PENDING_FOR_KYC)
                    && (Boolean.FALSE.equals(v1TenantSettingsResponse.getFetchRatesMandate()) || (!Objects.isNull(customerBooking.getBookingCharges()) && !customerBooking.getBookingCharges().isEmpty()))) {
                CustomerBooking finalCustomerBooking = customerBooking;
                CompletableFuture.runAsync(masterDataUtils.withMdc(() -> bookingIntegrationsUtility.createBookingInPlatform(finalCustomerBooking)), executorService);
            }
        } catch (Exception e) {
            log.error(e.getMessage());
        }

        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(customerBooking, CustomerBookingResponse.class));
    }

    private CustomerBooking updateEntities(CustomerBooking customerBooking, CustomerBookingRequest request, String oldEntity) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        populateTotalRevenueDetails(customerBooking, request);
        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        if (Objects.equals(customerBooking.getBookingStatus(), BookingStatus.READY_FOR_SHIPMENT) && !checkForCreditLimitManagement(customerBooking)) {
            throw new RunnerException("Request for credit limit has not been approved. Hence cannot proceed.");
        }

        customerBooking = customerBookingDao.save(customerBooking);
        Long bookingId = customerBooking.getId();

        List<PackingRequest> packingRequest = request.getPackingList();
        if (packingRequest != null)
            customerBooking.setPackingList(packingDao.updateEntityFromBooking(commonUtils.convertToEntityList(packingRequest, Packing.class), bookingId));

        List<RoutingsRequest> routingsRequest = request.getRoutingList();
        if (routingsRequest != null)
            customerBooking.setRoutingList(routingsDao.updateEntityFromBooking(commonUtils.convertToEntityList(routingsRequest, Routings.class), bookingId));

        List<ContainerRequest> containerRequest = request.getContainersList();
        if (containerRequest != null) {
            List<Containers> containers = containerDao.updateEntityFromBooking(commonUtils.convertToEntityList(containerRequest, Containers.class), bookingId);
            customerBooking.setContainersList(containers);
        }

        List<Containers> containers = customerBooking.getContainersList();
        Map<UUID, Containers> containerMap = new HashMap<>();
        if (containers != null && !containers.isEmpty()) {
            for (Containers container : containers) {
                containerMap.put(container.getGuid(), container);
            }
        }
        List<BookingChargesRequest> bookingChargesRequest = request.getBookingCharges();
        if (bookingChargesRequest != null) {
            List<BookingCharges> bookingCharges = bookingChargesDao.updateEntityFromBooking(commonUtils.convertToEntityList(bookingChargesRequest, BookingCharges.class), bookingId);
            customerBooking.setBookingCharges(bookingCharges);
        }
        if (Objects.equals(customerBooking.getBookingStatus(), BookingStatus.READY_FOR_SHIPMENT)) {
            if (Boolean.TRUE.equals(tenantSettingsResponse.getShipmentServiceV2Enabled())) {
                boolean hasAirDGPermission = UserContext.isAirDgUser();
                if (Objects.equals(request.getTransportType(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(request.getIsDg(), Boolean.TRUE) && !hasAirDGPermission) {
                    throw new ValidationException("User does not have AIR DG Permission to create AIR Shipment from Booking");
                }
                ShipmentDetailsResponse shipmentResponse = (ShipmentDetailsResponse) (((RunnerResponse) bookingIntegrationsUtility.createShipmentInV2(request).getBody()).getData());
                //Check 3
                if (shipmentResponse != null) {
                    if (customerBooking.getBookingCharges() != null && !customerBooking.getBookingCharges().isEmpty()) {
                        bookingIntegrationsUtility.createShipment(customerBooking, false, true, shipmentResponse, V1AuthHelper.getHeaders());
                    }
                    customerBooking.setShipmentId(shipmentResponse.getShipmentId());
                    customerBooking.setShipmentEntityIdV2(StringUtility.convertToString(shipmentResponse.getId()));
                    customerBooking.setShipmentGuid(StringUtility.convertToString(shipmentResponse.getGuid()));
                    customerBooking.setShipmentCreatedDate(LocalDateTime.now());
                    customerBooking = customerBookingDao.save(customerBooking);

                    mdmServiceAdapter.createShipmentTaskFromBooking(
                            CommonRequestModel.buildRequest(
                                    CreateShipmentTaskFromBookingTaskRequest.builder()
                                            .currentEntityType(CustomerBookingConstants.CUSTOMER_BOOKING_STRING)
                                            .currentEntityUuid(StringUtility.convertToString(customerBooking.getGuid()))
                                            .newEntityType(Constants.SHIPMENT)
                                            .newEntityUuid(StringUtility.convertToString(shipmentResponse.getGuid()))
                                            .build()));
                }
            } else {
                V1ShipmentCreationResponse shipmentCreationResponse = jsonHelper.convertValue(bookingIntegrationsUtility.createShipmentInV1(customerBooking, true, true, null, V1AuthHelper.getHeaders()).getBody(), V1ShipmentCreationResponse.class);
                if (!Objects.isNull(shipmentCreationResponse) && !Objects.isNull(shipmentCreationResponse.getShipmentId())) {
                    customerBooking.setShipmentId(shipmentCreationResponse.getShipmentId());
                    customerBooking.setShipmentEntityId(shipmentCreationResponse.getEntityId());
                    customerBooking.setShipmentGuid(shipmentCreationResponse.getShipmentGuid());
                    customerBooking.setShipmentCreatedDate(LocalDateTime.now());
                    customerBooking.setIsBillCreated(true);
                    customerBooking = customerBookingDao.save(customerBooking);
                }
            }
        }
        try {
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(customerBooking)
                            .prevData(jsonHelper.readFromJson(oldEntity, CustomerBooking.class))
                            .parent(CustomerBooking.class.getSimpleName())
                            .parentId(customerBooking.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );
        } catch (Exception e) {
            log.error(e.getMessage());
        }
        return customerBooking;
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Booking list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Pair<Specification<CustomerBooking>, Pageable> tuple = fetchData(request, CustomerBooking.class, tableNames);
            Page<CustomerBooking> customerBookingPage = customerBookingDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Booking list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(customerBookingPage.getContent()),
                    customerBookingPage.getTotalPages(),
                    customerBookingPage.getTotalElements());
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
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null || request.getId() == null) {
                log.debug("Request is empty for Booking delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            long id = request.getId();
            Optional<CustomerBooking> customerBooking = customerBookingDao.findById(id);
            if (!customerBooking.isPresent()) {
                log.debug(CustomerBookingConstants.BOOKING_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            customerBookingDao.delete(customerBooking.get());
            log.info("Deleted Booking details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            double _start = System.currentTimeMillis();
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null || (request.getId() == null && request.getGuid() == null)) {
                log.error("Request Id and Guid are null for Booking retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Long id = request.getId();
            Optional<CustomerBooking> customerBooking;
            if (id != null) {
                customerBooking = customerBookingDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                customerBooking = customerBookingDao.findByGuid(guid);
            }

            if (!customerBooking.isPresent()) {
                log.debug(CustomerBookingConstants.BOOKING_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            double current = System.currentTimeMillis();
            log.info("Booking details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            log.info("Time taken to fetch booking details from db: {} Request Id {}", current - _start, LoggerHelper.getRequestIdFromMDC());
            CustomerBookingResponse customerBookingResponse = jsonHelper.convertValue(customerBooking.get(), CustomerBookingResponse.class);
            double _next = System.currentTimeMillis();
            log.info("Time taken to fetch details from db: {} Request Id {}", _next - current, LoggerHelper.getRequestIdFromMDC());
            createCustomerBookingResponse(customerBooking.get(), customerBookingResponse);
            return ResponseHelper.buildSuccessResponse(customerBookingResponse);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> checkCreditLimitFromFusion(CommonRequestModel commonRequestModel) throws RunnerException {
        CreditLimitRequest creditLimitRequest = (CreditLimitRequest) commonRequestModel.getData();

        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();

        if (Boolean.FALSE.equals(v1TenantSettingsResponse.getEnableCreditLimitManagement()) || Boolean.FALSE.equals(v1TenantSettingsResponse.getIsCreditLimitWithFusionEnabled())) {
            log.error("EnableCreditLimitManagement Or EnableCreditLimitIntegrationWithFusion is False in Branch settings with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("EnableCreditLimitManagement Or EnableCreditLimitIntegrationWithFusion is False in Branch settings");
        }
        boolean isCustomerBookingRestricted = v1TenantSettingsResponse.getRestrictedItemsForCreditLimit().stream().anyMatch(p -> p.equals("CUS_BK"));
        if (!isCustomerBookingRestricted) {
            log.error("'Restrict the transaction when Credit Limit is enabled' does not include CustomerBooking with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("'Restrict the transaction when Credit Limit is enabled' does not include CustomerBooking");
        }
        CheckCreditBalanceFusionRequest request = CheckCreditBalanceFusionRequest.builder().req_Params(new CheckCreditBalanceFusionRequest.ReqParams()).build();
        if (v1TenantSettingsResponse.getCreditLimitOn() == 0) {

            if (creditLimitRequest != null && creditLimitRequest.getCustomerIdentifierId() == null && creditLimitRequest.getClientOrgCode() != null) {
                CommonV1ListRequest orgRequest = new CommonV1ListRequest();
                List<Object> orgField = new ArrayList<>(List.of("OrganizationCode"));
                String op = "=";
                List<Object> orgCriteria = new ArrayList<>(List.of(orgField, op, creditLimitRequest.getClientOrgCode()));
                orgRequest.setCriteriaRequests(orgCriteria);
                V1DataResponse orgResponse = v1Service.fetchOrganization(orgRequest);
                List<EntityTransferOrganizations> orgList = jsonHelper.convertValueToList(orgResponse.entities, EntityTransferOrganizations.class);


                long orgId = orgList.get(0).getId();
                creditLimitRequest.setCustomerIdentifierId(orgList.get(0).getCustomerIdentifier());
                List<Object> finalCriteria = new ArrayList<>();


                CommonV1ListRequest addressReq = new CommonV1ListRequest();
                List<Object> addressField = new ArrayList<>(List.of("AddressShortCode"));
                List<Object> addressCriteria = new ArrayList<>(List.of(addressField, op, creditLimitRequest.getClientAddressCode()));
                finalCriteria.add(addressCriteria);

                finalCriteria.add("and");

                List<Object> orgIdCriteria = new ArrayList<>();
                List<Object> orgIdField = List.of("OrgId");
                orgIdCriteria.add(List.of(orgIdField, op, orgId));
                finalCriteria.addAll(orgIdCriteria);

                addressReq.setCriteriaRequests(finalCriteria);
                V1DataResponse addressResponse = v1Service.addressList(addressReq);
                List<EntityTransferAddress> addressList = jsonHelper.convertValueToList(addressResponse.entities, EntityTransferAddress.class);
                creditLimitRequest.setSiteIdentifierId(addressList.get(0).getSiteIdentifier());


            }

            if (creditLimitRequest == null || creditLimitRequest.getCustomerIdentifierId() == null) {
                log.error("CustomerIdentifierId is Required with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException("CustomerIdentifierId is Required for credit check");
            }
            request.getReq_Params().setAccount_number(creditLimitRequest.getCustomerIdentifierId());
        } else if (v1TenantSettingsResponse.getCreditLimitOn() == 1) {
            if (creditLimitRequest == null || creditLimitRequest.getSiteIdentifierId() == null) {
                log.error("SiteIdentifierId is Required with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException("SiteIdentifierId is Required for credit check");
            }
            request.getReq_Params().setSite_number(creditLimitRequest.getSiteIdentifierId());
        }
        if (Boolean.TRUE.equals(v1TenantSettingsResponse.getIsGlobalFusionIntegrationEnabled())) {
            request.getReq_Params().setCalling_System(CustomerBookingConstants.GCR_FUSION);
            request.getReq_Params().setBu_id(v1TenantSettingsResponse.getBusinessUnitName());
            ResponseEntity<IRunnerResponse> response = fusionServiceAdapter.checkCreditLimitP100(CommonRequestModel.buildRequest(request));
            if (response == null || response.getBody() == null || ((DependentServiceResponse) response.getBody()).getData() == null) {
                log.error("No Data found on Fusion with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException("No Data found on Fusion");
            }
            CheckCreditBalanceFusionResponse checkCreditBalanceFusionResponse = modelMapper.map(((DependentServiceResponse) response.getBody()).getData(), CheckCreditBalanceFusionResponse.class);
            CheckCreditLimitResponse checkCreditLimitResponse = createCheckCreditLimitPayload(checkCreditBalanceFusionResponse);
            try {
                UpdateOrgCreditLimitBookingResponse updateOrgCreditLimitBookingResponse = jsonHelper.convertValue(bookingIntegrationsUtility.updateOrgCreditLimitFromBooking(checkCreditLimitResponse).getBody(), UpdateOrgCreditLimitBookingResponse.class);
                if (updateOrgCreditLimitBookingResponse.getSuccess()) {
                    log.info("Successfully Updated Org with Credit Limit in V1");
                } else {
                    log.error("Error in Updating Org Credit Limit in V1 with error : {}", updateOrgCreditLimitBookingResponse.getError());
                    throw new ValidationException("Error in Updating Org Credit Limit in V1 with error : " + updateOrgCreditLimitBookingResponse.getError());
                }
            } catch (Exception ex) {
                log.error("Error in Updating Org Credit Limit in V1 with error : {} with Request Id {}", ex.getMessage(), LoggerHelper.getRequestIdFromMDC());
                throw new RuntimeException("Error in Updating Org Credit Limit in V1 with error : " + ex.getMessage());
            }
            return ResponseHelper.buildSuccessResponse(checkCreditLimitResponse);
        } else {
            log.error("'Enable Global Fusion Integration' is false for this Tenant this is required for Customer Booking with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("'Enable Global Fusion Integration' is false for this Tenant this is required for Customer Booking");
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> retryForBilling(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (Objects.isNull(request) || Objects.isNull(request.getId()))
                log.error("Request Id is null for Booking retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());

            Optional<CustomerBooking> customerBookingOptional = customerBookingDao.findById(request.getId());
            if (customerBookingOptional.isEmpty()) {
                log.debug(CustomerBookingConstants.BOOKING_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            CustomerBooking customerBooking = customerBookingOptional.get();
            if (!Objects.equals(customerBooking.getBookingStatus(), BookingStatus.READY_FOR_SHIPMENT))
                throw new RunnerException(String.format("Booking should be in: %s stage for this operation", BookingStatus.READY_FOR_SHIPMENT));
            if (!Objects.isNull(customerBooking.getIsBillCreated()) && Boolean.TRUE.equals(customerBooking.getIsBillCreated()))
                throw new RunnerException(String.format("Bill is already created for booking with id: %s", request.getId()));

            V1ShipmentCreationResponse shipmentCreationResponse = jsonHelper.convertValue(bookingIntegrationsUtility.createShipmentInV1(customerBooking, false, true, UUID.fromString(customerBooking.getShipmentGuid()), V1AuthHelper.getHeaders()).getBody(), V1ShipmentCreationResponse.class);
            customerBooking.setIsBillCreated(true);
            customerBookingDao.save(customerBooking);
            return ResponseHelper.buildSuccessResponse(shipmentCreationResponse);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private CheckCreditLimitResponse createCheckCreditLimitPayload(CheckCreditBalanceFusionResponse checkCreditBalanceFusionResponse) {
        if (checkCreditBalanceFusionResponse.getData().getCreditDetails() == null || checkCreditBalanceFusionResponse.getData().getCreditDetails().isEmpty()) {
            log.error("No Data found on Fusion with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("No Data found on Fusion: " + checkCreditBalanceFusionResponse.getData().getMessage());
        }
        double totalCreditLimit = checkCreditBalanceFusionResponse.getData().getCreditDetails().get(0).getTotalCreditLimit();
        double outstandingAmount = checkCreditBalanceFusionResponse.getData().getCreditDetails().get(0).getOutstandingAmount();
        double overDueAmount = checkCreditBalanceFusionResponse.getData().getCreditDetails().get(0).getOverDue();
        double totalCreditAvailableBalance = (totalCreditLimit - outstandingAmount);
        var PaymentTerm = checkCreditBalanceFusionResponse.getData().getCreditDetails().get(0).getPaymentTerms();
        if (PaymentTerm == null || PaymentTerm.isEmpty()) {
            PaymentTerm = CustomerBookingConstants.IMMEDIATE;
        }

        double creditLimitUtilizedPer = totalCreditLimit != 0 ? (outstandingAmount * 100) / totalCreditLimit : 0;
        double overDuePer = totalCreditLimit != 0 ? (overDueAmount * 100) / totalCreditLimit : 0;
        var num = CommonUtils.roundOffToTwoDecimalPlace(checkCreditBalanceFusionResponse.getData().getCreditDetails().get(0).getTotalCreditLimit());
        return CheckCreditLimitResponse.builder()
                .totalCreditLimit(CommonUtils.roundOffToTwoDecimalPlace(totalCreditLimit))
                .currency(checkCreditBalanceFusionResponse.getData().getCreditDetails().get(0).getCreditLimitCurrency())
                .outstandingAmount(CommonUtils.roundOffToTwoDecimalPlace(outstandingAmount))
                .notDueAmount(CommonUtils.roundOffToTwoDecimalPlace(checkCreditBalanceFusionResponse.getData().getCreditDetails().get(0).getNotDue()))
                .overdueAmount(CommonUtils.roundOffToTwoDecimalPlace(overDueAmount))
                .totalCreditAvailableBalance(CommonUtils.roundOffToTwoDecimalPlace(totalCreditAvailableBalance))
                .creditLimitUtilizedPer(CommonUtils.roundOffToTwoDecimalPlace(creditLimitUtilizedPer))
                .overduePer(CommonUtils.roundOffToTwoDecimalPlace(overDuePer))
                .paymentTerms(PaymentTerm)
                .accountNumber(checkCreditBalanceFusionResponse.getData().getCreditDetails().get(0).getAccountNumber())
                .siteNumber(checkCreditBalanceFusionResponse.getData().getCreditDetails().get(0).getSiteNumber())
                .build();
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> platformCreateBooking(CommonRequestModel commonRequestModel) throws RunnerException {
        PlatformToRunnerCustomerBookingRequest request = (PlatformToRunnerCustomerBookingRequest) commonRequestModel.getData();
        if (request.getIsSingleUsageContract() != null)
            request.setContractStatus(request.getIsSingleUsageContract() ? "SINGLE_USAGE" : "MULTI_USAGE");
        String bookingNumber = request.getBookingNumber();
        if (bookingNumber == null) {
            log.error("Booking Number is empty for create Booking with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Optional<CustomerBooking> customerBooking = customerBookingDao.findByBookingNumber(bookingNumber);

        PlatformToRunnerCustomerBookingResponse platformResponse = new PlatformToRunnerCustomerBookingResponse();
        platformResponse.setBookingNumber(bookingNumber);
        setOrgAndAddressToParties(request);

        Map<String, UUID> referenceIdVsGuidContainerMap = new HashMap<>();

        if (request.getContainersList() != null) {
            List<PlatformToRunnerCustomerBookingResponse.ReferenceNumbersGuidMapResponse> referenceNumbersGuidMapResponses = new ArrayList<>();
            request.getContainersList().forEach(cont -> {
                if (cont.getGuid() == null)
                    cont.setGuid(UUID.randomUUID());
                referenceIdVsGuidContainerMap.put(cont.getReference_id(), cont.getGuid());
                referenceNumbersGuidMapResponses.add(PlatformToRunnerCustomerBookingResponse.ReferenceNumbersGuidMapResponse.builder()
                        .reference_id(cont.getReference_id())
                        .guid(cont.getGuid())
                        .build());
            });
            platformResponse.setContainers(referenceNumbersGuidMapResponses);
        }

        if (request.getPackingList() != null) {
            List<PlatformToRunnerCustomerBookingResponse.ReferenceNumbersGuidMapResponse> referenceNumbersGuidMapResponses = new ArrayList<>();
            request.getPackingList().forEach(pack -> {
                pack.setLengthUnit(pack.getDimensionUnit());
                pack.setWidthUnit(pack.getDimensionUnit());
                pack.setHeightUnit(pack.getDimensionUnit());
                pack.setIsDimension(true);
                if (pack.getGuid() == null)
                    pack.setGuid(UUID.randomUUID());
                referenceNumbersGuidMapResponses.add(PlatformToRunnerCustomerBookingResponse.ReferenceNumbersGuidMapResponse.builder()
                        .reference_id(pack.getReference_id())
                        .guid(pack.getGuid())
                        .build());
            });
            platformResponse.setPacks(referenceNumbersGuidMapResponses);
        }

        if (request.getRoutingList() != null) {
            List<PlatformToRunnerCustomerBookingResponse.ReferenceNumbersGuidMapResponse> referenceNumbersGuidMapResponses = new ArrayList<>();
            request.getRoutingList().forEach(route -> {
                if (route.getGuid() == null)
                    route.setGuid(UUID.randomUUID());
                referenceNumbersGuidMapResponses.add(PlatformToRunnerCustomerBookingResponse.ReferenceNumbersGuidMapResponse.builder()
                        .reference_id(route.getReference_id())
                        .guid(route.getGuid())
                        .build());
            });
            platformResponse.setRoutings(referenceNumbersGuidMapResponses);
        }

        if (request.getBookingCharges() != null) {
            List<PlatformToRunnerCustomerBookingResponse.ReferenceNumbersGuidMapResponse> referenceNumbersGuidMapResponses = new ArrayList<>();
            request.getBookingCharges().forEach(charge -> {
                if (charge.getGuid() == null)
                    charge.setGuid(UUID.randomUUID());
                referenceNumbersGuidMapResponses.add(PlatformToRunnerCustomerBookingResponse.ReferenceNumbersGuidMapResponse.builder()
                        .reference_id(charge.getReference_id())
                        .guid(charge.getGuid())
                        .build());

//                if (charge.getContainers() != null) {
//                    charge.getContainers().forEach(cont -> {
//                        if (cont.getRunner_guid() != null) {
//                            if (charge.getContainersUUID() != null)
//                                charge.getContainersUUID().add(cont.getRunner_guid());
//                            else
//                                charge.setContainersUUID(List.of(cont.getRunner_guid()));
//                        } else {
//                            if (charge.getContainersUUID() != null) {
//                                if (referenceIdVsGuidContainerMap.containsKey(cont.getReference_id())) {
//                                    charge.getContainersUUID().add(referenceIdVsGuidContainerMap.get(cont.getReference_id()));
//                                }
//                            } else {
//                                if (referenceIdVsGuidContainerMap.containsKey(cont.getReference_id())) {
//                                    charge.setContainersUUID(List.of(referenceIdVsGuidContainerMap.get(cont.getReference_id())));
//                                }
//                            }
//                        }
//                    });
//                }
            });
            platformResponse.setCharges(referenceNumbersGuidMapResponses);
        }
        if (request.getBookingStatus() == null && request.getSource() != null && BookingSource.B2B.equals(request.getSource()))
            request.setBookingStatus(BookingStatus.PENDING_FOR_REVIEW);

        CustomerBookingRequest customerBookingRequest = modelMapper.map(request, CustomerBookingRequest.class);
        assignCarrierDetailsToRequest(customerBookingRequest, request);
        ResponseEntity<RunnerResponse<CustomerBookingResponse>> response = null;
        if (customerBooking.isEmpty()) {
            customerBookingRequest.setCurrentPartyForQuote("CLIENT");
            this.createPlatformBooking(customerBookingRequest);
        } else {
            if (Objects.equals(customerBooking.get().getBookingStatus(), BookingStatus.READY_FOR_SHIPMENT))
                throw new ValidationException("Booking alterations are not allowed once booking moved to Ready For Shipment.");
            if (customerBooking.get().getId() != null)
                customerBookingRequest.setId(customerBooking.get().getId());
            if (customerBooking.get().getGuid() != null)
                customerBookingRequest.setGuid(customerBooking.get().getGuid());
            if (customerBooking.get().getCarrierDetails() != null) {
                customerBookingRequest.getCarrierDetails().setId(customerBooking.get().getCarrierDetails().getId());
                customerBookingRequest.getCarrierDetails().setGuid(customerBooking.get().getCarrierDetails().getGuid());
            }
            if (customerBooking.get().getCurrentPartyForQuote() != null) {
                customerBookingRequest.setCurrentPartyForQuote(customerBooking.get().getCurrentPartyForQuote());
            }

            Map<UUID, Long> guidVsIdContainerMap = new HashMap<>();
            Map<UUID, Long> guidVsIdPackingMap = new HashMap<>();
            Map<UUID, Long> guidVsIdRoutingMap = new HashMap<>();
            Map<UUID, Long> guidVsIdChargesMap = new HashMap<>();
            if (customerBooking.get().getContainersList() != null) {
                customerBooking.get().getContainersList().forEach(cont -> {
                    guidVsIdContainerMap.put(cont.getGuid(), cont.getId());
                });
            }

            if (customerBooking.get().getPackingList() != null) {
                customerBooking.get().getPackingList().forEach(pack -> {
                    guidVsIdPackingMap.put(pack.getGuid(), pack.getId());
                });
            }

            if (customerBooking.get().getRoutingList() != null) {
                customerBooking.get().getRoutingList().forEach(route -> {
                    guidVsIdRoutingMap.put(route.getGuid(), route.getId());
                });
            }

            if (customerBooking.get().getBookingCharges() != null) {
                customerBooking.get().getBookingCharges().forEach(charge -> {
                    guidVsIdChargesMap.put(charge.getGuid(), charge.getId());
                });
            }

            if (customerBookingRequest.getContainersList() != null) {
                customerBookingRequest.getContainersList().forEach(cont -> {
                    if (cont.getGuid() != null) {
                        if (guidVsIdContainerMap.containsKey(cont.getGuid()))
                            cont.setId(guidVsIdContainerMap.get(cont.getGuid()));
                    }
                });
            }

            if (customerBookingRequest.getPackingList() != null) {
                customerBookingRequest.getPackingList().forEach(pack -> {
                    if (pack.getGuid() != null) {
                        if (guidVsIdPackingMap.containsKey(pack.getGuid()))
                            pack.setId(guidVsIdPackingMap.get(pack.getGuid()));
                    }
                });
            }

            if (customerBookingRequest.getRoutingList() != null) {
                customerBookingRequest.getRoutingList().forEach(route -> {
                    if (route.getGuid() != null) {
                        if (guidVsIdRoutingMap.containsKey(route.getGuid()))
                            route.setId(guidVsIdRoutingMap.get(route.getGuid()));
                    }
                });
            }

            if (customerBookingRequest.getBookingCharges() != null) {
                customerBookingRequest.getBookingCharges().forEach(charge -> {
                    if (charge.getGuid() != null) {
                        if (guidVsIdChargesMap.containsKey(charge.getGuid()))
                            charge.setId(guidVsIdChargesMap.get(charge.getGuid()));
                    }
                });
            }

            this.updatePlatformBooking(customerBookingRequest, customerBooking.get());
        }

        return ResponseHelper.buildSuccessResponse(platformResponse);
    }

    private void setOrgAndAddressToParties(PlatformToRunnerCustomerBookingRequest request) {
        Map<String, PartiesRequest> requestMap = new HashMap<>();
        if (request.getCustomer() != null) {
            requestMap.put(CUSTOMER_REQUEST, request.getCustomer());
        }
        if (request.getConsignor() != null && !Boolean.TRUE.equals(request.getIsConsignorFreeText()) &&
                !IsStringNullOrEmpty(request.getConsignor().getOrgCode()) &&
                !IsStringNullOrEmpty(request.getConsignor().getAddressCode())) {
            requestMap.put(CONSIGNOR_REQUEST, request.getConsignor());
        } else {
            transformOrgAndAddressToRawData(request.getConsignor());
        }
        if (request.getConsignee() != null && !Boolean.TRUE.equals(request.getIsConsigneeFreeText()) &&
                !IsStringNullOrEmpty(request.getConsignee().getOrgCode()) &&
                !IsStringNullOrEmpty(request.getConsignee().getAddressCode())) {
            requestMap.put(CONSIGNEE_REQUEST, request.getConsignee());
        } else {
            transformOrgAndAddressToRawData(request.getConsignee());
        }
        if (request.getNotifyParty() != null && !Boolean.TRUE.equals(request.getIsNotifyPartyFreeText()) &&
                !IsStringNullOrEmpty(request.getNotifyParty().getOrgCode()) &&
                !IsStringNullOrEmpty(request.getNotifyParty().getAddressCode())) {
            requestMap.put(NOTIFY_PARTY_REQUEST, request.getNotifyParty());
        } else {
            transformOrgAndAddressToRawData(request.getNotifyParty());
        }
        bookingIntegrationsUtility.transformOrgAndAddressPayloadToGivenParties(requestMap);
        if (requestMap.containsKey("Customer"))
            request.setCustomer(requestMap.get("Customer"));
        if (requestMap.containsKey("Consignor"))
            request.setConsignor(requestMap.get("Consignor"));
        if (requestMap.containsKey("Consignee"))
            request.setConsignee(requestMap.get("Consignee"));
        if (requestMap.containsKey("Notify Party"))
            request.setNotifyParty(requestMap.get("Notify Party"));

        if (request.getBookingCharges() != null && !request.getBookingCharges().isEmpty()) {
            request.getBookingCharges().forEach(charge -> {
                if (charge.getCreditor() != null) {
                    String orgCode = charge.getCreditor().getOrgCode();
                    String addressCode = charge.getCreditor().getAddressCode();
                    bookingIntegrationsUtility.transformOrgAndAddressPayload(charge.getCreditor(), addressCode, orgCode);
                }
                if (charge.getDebtor() != null) {
                    String orgCode = charge.getDebtor().getOrgCode();
                    String addressCode = charge.getDebtor().getAddressCode();
                    bookingIntegrationsUtility.transformOrgAndAddressPayload(charge.getDebtor(), addressCode, orgCode);
                } else {
                    if (charge.getCreditor() != null)
                        charge.setDebtor(charge.getCreditor());
                }
            });

        }
    }

    private void transformOrgAndAddressToRawData(PartiesRequest partiesRequest) {
        if (partiesRequest == null)
            return;
        Map<String, Object> orgData = partiesRequest.getOrgData();
        Map<String, Object> addressData = partiesRequest.getAddressData();

        String orgString = "";
        String addressString = "";
        if (orgData.containsKey(PartiesConstants.FULLNAME)) {
            orgString = orgString.concat((String) orgData.get(PartiesConstants.FULLNAME));
            addressString = addressString.concat(orgData.get(PartiesConstants.FULLNAME) + "|");
        }
        partiesRequest.setOrgData(new HashMap<>(Map.of(PartiesConstants.RAW_DATA, orgString)));
        partiesRequest.getOrgData().putAll(orgData);
        if (addressData.containsKey(PartiesConstants.ADDRESS1)) {
            addressString = addressString.concat(addressData.get(PartiesConstants.ADDRESS1) + "|");
        }
        if (addressData.containsKey(PartiesConstants.ADDRESS2)) {
            addressString = addressString.concat(addressData.get(PartiesConstants.ADDRESS2) + "|");
        }
        if (addressData.containsKey(PartiesConstants.CITY)) {
            addressString = addressString.concat(addressData.get(PartiesConstants.CITY) + "|");
        }
        if (addressData.containsKey(PartiesConstants.STATE)) {
            addressString = addressString.concat(addressData.get(PartiesConstants.STATE) + "|");
        }
        if (addressData.containsKey(PartiesConstants.COUNTRY)) {
            addressString = addressString.concat(addressData.get(PartiesConstants.COUNTRY) + "|");
        }
        if (addressData.containsKey(PartiesConstants.ZIP_POST_CODE)) {
            addressString = addressString.concat(addressData.get(PartiesConstants.ZIP_POST_CODE) + "|");
        }
        if (addressData.containsKey(PartiesConstants.MOBILE)) {
            addressString = addressString.concat(addressData.get(PartiesConstants.MOBILE) + "|");
        }
        if (addressData.containsKey(PartiesConstants.PHONE)) {
            addressString = addressString.concat(addressData.get(PartiesConstants.PHONE) + "|");
        }
        partiesRequest.setIsAddressFreeText(true);
        partiesRequest.setAddressData(new HashMap<>(Map.of(PartiesConstants.RAW_DATA, addressString)));
        partiesRequest.getAddressData().putAll(addressData);
    }

    private CustomerBookingResponse updatePlatformBooking(CustomerBookingRequest request, CustomerBooking oldEntity) throws RunnerException {
        CustomerBooking customerBooking = jsonHelper.convertValue(request, CustomerBooking.class);
        customerBooking.setIsPlatformBookingCreated(Boolean.TRUE);
        customerBooking.setSource(BookingSource.Platform);
        try {
            customerBooking = this.updateEntities(customerBooking, request, jsonHelper.convertToJson(oldEntity));
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RunnerException(e.getMessage());
        }
        return jsonHelper.convertValue(customerBooking, CustomerBookingResponse.class);
    }

    private CustomerBookingResponse createPlatformBooking(CustomerBookingRequest request) throws RunnerException {
        if (request == null) {
            log.error("Request is null for Customer Booking Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        CustomerBooking customerBooking = jsonHelper.convertValue(request, CustomerBooking.class);
        customerBooking.setIsConsigneeAddressFreeText(customerBooking.getIsConsigneeFreeText() != null && customerBooking.getIsConsigneeFreeText());
        customerBooking.setIsConsignorAddressFreeText(customerBooking.getIsConsignorFreeText() != null && customerBooking.getIsConsignorFreeText());
        customerBooking.setIsCustomerAddressFreeText(false);
        customerBooking.setIsNotifyPartyAddressFreeText(customerBooking.getIsNotifyPartyFreeText() != null && customerBooking.getIsNotifyPartyFreeText());
        customerBooking.setSource(BookingSource.Platform);
        customerBooking.setIsPlatformBookingCreated(Boolean.TRUE);
        try {
            createEntities(customerBooking, request);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RunnerException(e.getMessage());
        }
        return jsonHelper.convertValue(customerBooking, CustomerBookingResponse.class);
    }

    public VesselsResponse getVesselsData(String name) {
        List<Object> vesselCriteria = Arrays.asList(
                List.of("Name"),
                "=",
                name
        );
        CommonV1ListRequest vesselRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(vesselCriteria).build();
        V1DataResponse vesselResponse = v1Service.fetchVesselData(vesselRequest);
        List<VesselsResponse> vesselsResponse = jsonHelper.convertValueToList(vesselResponse.entities, VesselsResponse.class);
        if (vesselsResponse != null && !vesselsResponse.isEmpty())
            return vesselsResponse.get(0);
        return null;
    }

    private void assignCarrierDetailsToRequest(CustomerBookingRequest customerBookingRequest, PlatformToRunnerCustomerBookingRequest request) {

        String vessel = null;
        if (!IsStringNullOrEmpty(request.getVessel())) {
            VesselsResponse vesselsResponse = getVesselsData(request.getVessel());
            if (vesselsResponse != null)
                vessel = StringUtility.convertToString(vesselsResponse.getGuid());
        }
        CarrierDetailRequest carrierDetailRequest = CarrierDetailRequest.builder()
                .origin(request.getOrigin())
                .destination(request.getDestination())
                .originPort(request.getOriginPort())
                .destinationPort(request.getDestinationPort())
                .shippingLine(getCarrierItemValueFromSCAC(request.getShippingLine()))
                .maxTransitHours(request.getMaxTransitHours())
                .minTransitHours(request.getMinTransitHours())
                .vessel(vessel)
                .voyage(request.getVoyage())
                .build();

        customerBookingRequest.setCarrierDetails(carrierDetailRequest);
    }

    private String getCarrierItemValueFromSCAC(String carrierSCACCode) {
        if (IsStringNullOrEmpty(carrierSCACCode))
            return null;
        List<String> carrierCodes = new ArrayList<>();
        carrierCodes.add(carrierSCACCode);
        Map<String, EntityTransferCarrier> map = masterDataUtils.fetchInBulkCarriersBySCACCode(carrierCodes);
        if (map.containsKey(carrierSCACCode))
            return map.get(carrierSCACCode).ItemValue;
        return null;
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<CustomerBooking> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(customerBooking -> {
            CustomerBookingResponse response = modelMapper.map(customerBooking, CustomerBookingResponse.class);
            responseList.add(response);
        });
        masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID);
        return responseList;
    }

    private void contractUtilisationForUpdate(CustomerBooking customerBooking, CustomerBooking old) throws RunnerException {
        if (!Objects.isNull(customerBooking.getContractId()) && Objects.equals(old.getContractId(), customerBooking.getContractId())) {
            // Alteration on same contract
            _npmContractUpdate(customerBooking, old, true, CustomerBookingConstants.REMOVE, false);
        } else if (!Objects.isNull(customerBooking.getContractId()) && !Objects.isNull(old.getContractId())) {
            // Lock current contract with current containers
            _npmContractUpdate(customerBooking, null, false, CustomerBookingConstants.REMOVE, false);
            // Release existing booking with old containers
            _npmContractUpdate(old, null, false, CustomerBookingConstants.ADD, false);
        } else if (!Objects.isNull(customerBooking.getContractId()) && Objects.isNull(old.getContractId())) {
            // Lock current contract with current containers
            _npmContractUpdate(customerBooking, null, false, CustomerBookingConstants.REMOVE, false);
        } else if (Objects.isNull(customerBooking.getContractId()) && !Objects.isNull(old.getContractId())) {
            // Release existing booking with old containers
            _npmContractUpdate(old, null, false, CustomerBookingConstants.ADD, false);
        }

        if (!Objects.isNull(customerBooking.getContractId()) && Objects.equals(customerBooking.getBookingStatus(), BookingStatus.CANCELLED)) {
            _npmContractUpdate(customerBooking, null, false, CustomerBookingConstants.ADD, true);
        }
    }

    private void _npmContractUpdate(CustomerBooking current, CustomerBooking old, Boolean isAlteration, String operation, boolean isCancelled) throws RunnerException {
        if (Objects.equals(current.getTransportType(), Constants.TRANSPORT_MODE_SEA) && !Objects.isNull(current.getContractId())) {
            List<LoadInfoRequest> loadInfoRequestList = containersListForLoad(current, old, operation);

            if (!loadInfoRequestList.isEmpty() || !isAlteration) {
                String contractStatus = null;
                if (Objects.equals(current.getContractStatus(), CustomerBookingConstants.SINGLE_USAGE) && isCancelled)
                    contractStatus = CustomerBookingConstants.ENABLED;
                else if (Objects.equals(current.getContractStatus(), CustomerBookingConstants.SINGLE_USAGE))
                    contractStatus = CustomerBookingConstants.DISABLED;

                UpdateContractRequest updateContractRequest = UpdateContractRequest.builder()
                        .contract_id(current.getContractId())
                        .contract_state(contractStatus)
                        .source(CustomerBookingConstants.RUNNER)
                        .source_type(CustomerBookingConstants.RUNNER)
                        .business_info(UpdateContractRequest.BusinessInfo.builder().product_name(current.getCargoType()).build())
                        .loads_info(loadInfoRequestList)
                        .is_alteration(isAlteration)
                        .build();

                npmService.updateContracts(CommonRequestModel.buildRequest(updateContractRequest));
            }
        }
    }

    private List<LoadInfoRequest> containersListForLoad(CustomerBooking current, CustomerBooking old, String operation) {
        Map<String, Containers> idVsContainerMap = new HashMap<>();
        List<LoadInfoRequest> loadInfoRequestList = new ArrayList<>();

        if (!Objects.isNull(old) && !Objects.isNull(old.getContainersList()))
            idVsContainerMap = old.getContainersList().stream().collect(Collectors.toMap(x -> x.getId() + "-" + x.getContainerCode() + "-" + x.getCommodityGroup(), x -> x));

        if (idVsContainerMap.isEmpty()) {
            // Only current operation, no comparison
            if (current.getContainersList() != null) {
                current.getContainersList().forEach(cont -> {
                    loadInfoRequestList.add(containerLoadConstruct(cont, operation, cont.getContainerCount()));
                });
            }
        } else {
            // Find delta
            Map<String, Containers> finalIdVsContainerMap = idVsContainerMap;
            if (current.getContainersList() != null) {
                current.getContainersList().forEach(cont -> {
                    String key = cont.getId() + "-" + cont.getContainerCode() + "-" + cont.getCommodityGroup();
                    if (finalIdVsContainerMap.containsKey(key)) {
                        // existing container with probably quantity change
                        if (finalIdVsContainerMap.get(key).getContainerCount() != cont.getContainerCount()) {
                            Long _diff = cont.getContainerCount() - finalIdVsContainerMap.get(key).getContainerCount();
                            loadInfoRequestList.add(containerLoadConstruct(cont, _diff > 0 ? CustomerBookingConstants.REMOVE : CustomerBookingConstants.ADD, Math.abs(_diff)));
                        }
                        finalIdVsContainerMap.remove(key);
                    } else {
                        // New Container
                        loadInfoRequestList.add(containerLoadConstruct(cont, CustomerBookingConstants.REMOVE, cont.getContainerCount()));
                    }
                });
            }
            // Release all the remaining loads
            finalIdVsContainerMap.forEach((k, v) -> loadInfoRequestList.add(containerLoadConstruct(v, CustomerBookingConstants.ADD, v.getContainerCount())));
        }
        return loadInfoRequestList;
    }

    private LoadInfoRequest containerLoadConstruct(Containers container, String operation, Long quantity) {
        LoadInfoRequest loadInfoRequest = new LoadInfoRequest();
        loadInfoRequest.setOperation(operation);
        loadInfoRequest.setLoad_details(LoadDetailsRequest.builder()
                .load_type(Constants.CARGO_TYPE_FCL)
                .cargo_type(container.getContainerCode())
                .product_category_code(container.getCommodityGroup())
                .hazardous_info(HazardousInfoRequest.builder().is_hazardous(false).build())
                .build());
        loadInfoRequest.setLoad_attributes(LoadAttributesRequest.builder()
                .quantity(quantity)
                .quantity_uom(CustomerBookingConstants.UNIT)
                .build());
        return loadInfoRequest;
    }

    private String generateBookingNumber(String cargoType) {
        String prefix = "DBAR";
        if (Objects.equals(cargoType, "FCL") || Objects.equals(cargoType, "FTL"))
            prefix = "DBFC";
        else if (Objects.equals(cargoType, "BBK") || Objects.equals(cargoType, "ROR") || Objects.equals(cargoType, "LCL") || Objects.equals(cargoType, "LTL"))
            prefix = "DBLC";
        return prefix + "-" + getRandomNumberString(7) + "-" + getRandomNumberString(6);
    }

    /**
     * To be used to fetch dependent master-data*
     *
     * @param customerBooking
     * @param customerBookingResponse
     */
    private void createCustomerBookingResponse(CustomerBooking customerBooking, CustomerBookingResponse customerBookingResponse) {
        try {
            double _start = System.currentTimeMillis();
            var masterListFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllMasterDataInSingleCall(customerBookingResponse)), executorService);
            var unLocationsFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllLocationDataInSingleCall(customerBookingResponse)), executorService);
            var vesselsFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllVesselDataInSingleCall(customerBookingResponse)), executorService);
            var carrierFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllCarrierDataInSingleCall(customerBookingResponse)), executorService);
            var containerTypeFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllContainerTypesInSingleCall(customerBookingResponse)), executorService);
            var chargeTypeFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllChargeTypesInSingleCall(customerBookingResponse)), executorService);
            if (customerBookingResponse.getBookingStatus() == BookingStatus.READY_FOR_SHIPMENT) {
                V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
                Boolean isShipmentV2 = tenantSettingsResponse.getShipmentServiceV2Enabled();
                if (isShipmentV2) {
                    if (customerBookingResponse.getShipmentEntityIdV2() == null) {
                        if (customerBookingResponse.getShipmentGuid() == null) {
                            if (customerBookingResponse.getShipmentEntityId() != null) {
                                V1RetrieveRequest v1RetrieveRequest =
                                        V1RetrieveRequest.builder().
                                                EntityId(customerBookingResponse.getShipmentEntityId()).
                                                build();
                                V1RetrieveResponse v1RetrieveResponse = v1Service.getShipment(v1RetrieveRequest);
                                ShipmentRetrieveResponse shipmentRetrieveResponse = (ShipmentRetrieveResponse) v1RetrieveResponse.getEntity();
                                var shipment = shipmentDao.findByGuid(shipmentRetrieveResponse.getGuid());
                                if (shipment.isPresent())
                                    customerBookingResponse.setShipmentEntityIdV2(StringUtility.convertToString(shipment.get().getId()));
                            }
                        } else {
                            var shipment = shipmentDao.findByGuid(UUID.fromString(customerBooking.getShipmentGuid()));
                            if (shipment.isPresent())
                                customerBookingResponse.setShipmentEntityIdV2(StringUtility.convertToString(shipment.get().getId()));
                        }
                    }
                } else {
                    if (customerBookingResponse.getShipmentEntityId() == null) {
                        if (customerBookingResponse.getShipmentGuid() != null) {
                            ShipmentBillingListRequest shipmentBillingListRequest = ShipmentBillingListRequest.builder().guidsList(List.of(UUID.fromString(customerBookingResponse.getShipmentGuid()))).build();
                            ShipmentBillingListResponse shipmentBillingListResponse = v1Service.fetchShipmentBillingData(shipmentBillingListRequest);
                            if (shipmentBillingListResponse.getData() != null && !shipmentBillingListResponse.getData().isEmpty()) {
                                ShipmentBillingListResponse.BillingData billingData = shipmentBillingListResponse.getData().get(customerBookingResponse.getShipmentGuid());
                                customerBookingResponse.setShipmentEntityId(StringUtility.convertToString(billingData.getId()));
                            }
                        }
                    }
                }
            }
            CompletableFuture.allOf(masterListFuture, unLocationsFuture, vesselsFuture, carrierFuture, containerTypeFuture, chargeTypeFuture).join();
            log.info("Time taken to fetch Master-data from V1: {} ms. || RequestId: {}", System.currentTimeMillis() - _start, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception ex) {
            log.error("Exception during fetching master data in retrieve API for booking number: {} with exception: {}", customerBooking.getBookingNumber(), ex.getMessage());
        }
    }

    public Runnable withMdc(Runnable runnable) {
        Map<String, String> mdc = MDC.getCopyOfContextMap();
        String token = RequestAuthContext.getAuthToken();
        var userContext = UserContext.getUser();
        return () -> {
            try {
                MDC.setContextMap(mdc);
                RequestAuthContext.setAuthToken(token);
                UserContext.setUser(userContext);
                runnable.run();
            } finally {
                RequestAuthContext.removeToken();
                MDC.clear();
                UserContext.removeUser();
            }
        };
    }

    private void populateTotalRevenueDetails(CustomerBooking customerBooking, CustomerBookingRequest request) {
        BigDecimal totalRevenue = BigDecimal.ZERO;
        if (request != null && request.getBookingCharges() != null) {
            totalRevenue = request.getBookingCharges().stream()
                    .filter(Objects::nonNull)
                    .map(c -> c.getLocalSellAmount() != null ? c.getLocalSellAmount() : BigDecimal.ZERO)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
        }
        customerBooking.setTotalRevenue(totalRevenue);
    }

    //    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCall(CustomerBookingResponse customerBookingResponse) {
        try {
            // Preprocessing
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>(masterDataUtils.createInBulkMasterListRequest(customerBookingResponse, CustomerBooking.class, fieldNameKeyMap, CustomerBooking.class.getSimpleName(), cacheMap));
            if (!Objects.isNull(customerBookingResponse.getRoutingList()))
                customerBookingResponse.getRoutingList().forEach(r -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId(), cacheMap)));
            if (!Objects.isNull(customerBookingResponse.getContainersList()))
                customerBookingResponse.getContainersList().forEach(c -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(c, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + c.getId(), cacheMap)));
            if (!Objects.isNull(customerBookingResponse.getPackingList()))
                customerBookingResponse.getPackingList().forEach(c -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(c, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + c.getId(), cacheMap)));
            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            // fetching from V1 in single call
            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            // Postprocessing
            customerBookingResponse.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CustomerBooking.class.getSimpleName()), CacheConstants.MASTER_LIST, true, cacheMap));
            if (!Objects.isNull(customerBookingResponse.getRoutingList()))
                customerBookingResponse.getRoutingList().forEach(r -> r.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Routings.class.getSimpleName() + r.getId()), CacheConstants.MASTER_LIST, true, cacheMap)));
            if (!Objects.isNull(customerBookingResponse.getContainersList()))
                customerBookingResponse.getContainersList().forEach(c -> c.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + c.getId()), CacheConstants.MASTER_LIST, true, cacheMap)));
            if (!Objects.isNull(customerBookingResponse.getPackingList()))
                customerBookingResponse.getPackingList().forEach(c -> c.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Packing.class.getSimpleName() + c.getId()), CacheConstants.MASTER_LIST, true, cacheMap)));
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CustomerBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }

    }

    //    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllLocationDataInSingleCall(CustomerBookingResponse customerBookingResponse) {
        try {
            // Preprocessing
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>();
            if (!Objects.isNull(customerBookingResponse.getCarrierDetails()))
                locationCodes.addAll((masterDataUtils.createInBulkUnLocationsRequest(customerBookingResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap)));
            if (!Objects.isNull(customerBookingResponse.getRoutingList()))
                customerBookingResponse.getRoutingList().forEach(r -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(r, Routings.class, fieldNameKeyMap, Routings.class.getSimpleName() + r.getId(), cacheMap)));
            // fetching from V1 in single call
            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);
            // Postprocessing
            if (!Objects.isNull(customerBookingResponse.getCarrierDetails()))
                customerBookingResponse.getCarrierDetails().setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.UNLOCATIONS, true, cacheMap));
            if (!Objects.isNull(customerBookingResponse.getRoutingList()))
                customerBookingResponse.getRoutingList().forEach(r -> r.setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Routings.class.getSimpleName() + r.getId()), CacheConstants.UNLOCATIONS, true, cacheMap)));

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllLocationDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CustomerBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    //    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllChargeTypesInSingleCall(CustomerBookingResponse customerBookingResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> chargeTypes = new HashSet<>();

            if (!Objects.isNull(customerBookingResponse.getBookingCharges()))
                customerBookingResponse.getBookingCharges().forEach(r -> chargeTypes.addAll(masterDataUtils.createInBulkChargeTypeRequest(r, BookingCharges.class, fieldNameKeyMap, BookingCharges.class.getSimpleName() + r.getId(), cacheMap)));
            Map<String, EntityTransferChargeType> v1Data = masterDataUtils.fetchInBulkChargeTypes(chargeTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.CHARGE_TYPE, chargeTypes, new EntityTransferChargeType(), cacheMap);

            if (!Objects.isNull(customerBookingResponse.getBookingCharges()))
                customerBookingResponse.getBookingCharges().forEach(r -> r.setChargeTypeMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(BookingCharges.class.getSimpleName() + r.getId()), CacheConstants.CHARGE_TYPE, true, cacheMap)));

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllChargeTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CustomerBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }

    }

    //    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllContainerTypesInSingleCall(CustomerBookingResponse customerBookingResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> containerTypes = new HashSet<>();
            if (!Objects.isNull(customerBookingResponse.getContainersList()))
                customerBookingResponse.getContainersList().forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(r, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + r.getId(), cacheMap)));

            Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferContainerType(), cacheMap);

            if (!Objects.isNull(customerBookingResponse.getContainersList()))
                customerBookingResponse.getContainersList().forEach(r -> r.setContainerCodeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + r.getId()), CacheConstants.CONTAINER_TYPE, true, cacheMap)));

            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllContainerTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CustomerBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    //    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllVesselDataInSingleCall(CustomerBookingResponse customerBookingResponse) {
        try {
            if (!Objects.isNull(customerBookingResponse.getCarrierDetails())) {
                Map<String, Object> cacheMap = new HashMap<>();
                Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
                Set<String> vesselList = new HashSet<>(masterDataUtils.createInBulkVesselsRequest(customerBookingResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap));
                Map v1Data = masterDataUtils.fetchInBulkVessels(vesselList);
                masterDataUtils.pushToCache(v1Data, CacheConstants.VESSELS, vesselList, new EntityTransferVessels(), cacheMap);
                customerBookingResponse.getCarrierDetails().setVesselsMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.VESSELS, true, cacheMap));
            }
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(List.of()));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllVesselDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CustomerBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    //    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCarrierDataInSingleCall(CustomerBookingResponse customerBookingResponse) {
        try {
            if (!Objects.isNull(customerBookingResponse.getCarrierDetails())) {
                Map<String, Object> cacheMap = new HashMap<>();
                Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
                Set<String> carriersList = new HashSet<>(masterDataUtils.createInBulkCarriersRequest(customerBookingResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap));
                Map<String, EntityTransferCarrier> v1Data = masterDataUtils.fetchInBulkCarriers(carriersList);
                masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER, carriersList, new EntityTransferCarrier(), cacheMap);
                customerBookingResponse.getCarrierDetails().setCarrierMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.CARRIER, true, cacheMap));
            }
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(List.of()));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCarrierDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CustomerBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> cloneBooking(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();

            if (request.getId() == null) {
                log.error("Request Id is null for booking cloning with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException("Booking Id cannot be null");
            }
            long id = request.getId();
            Optional<CustomerBooking> customerBooking = customerBookingDao.findById(id);
            if (!customerBooking.isPresent()) {
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            CustomerBookingResponse customerBookingResponse = jsonHelper.convertValue(customerBooking.get(), CustomerBookingResponse.class);
            customerBookingResponse.setId(null);
            customerBookingResponse.setGuid(null);
            customerBookingResponse.setBookingNumber(null);
            customerBookingResponse.setBookingStatus(BookingStatus.PENDING_FOR_KYC);
            customerBookingResponse.setSource(null);
            customerBookingResponse.setCreatedBy(null);
            customerBookingResponse.setSourceGuid(customerBooking.get().getGuid());
            customerBookingResponse.setBookingDate(LocalDateTime.now());
            if (customerBookingResponse.getCustomer() != null) {
                customerBookingResponse.getCustomer().setId(null);
                customerBookingResponse.getCustomer().setGuid(null);
            }
            if (customerBookingResponse.getConsignee() != null) {
                customerBookingResponse.getConsignee().setId(null);
                customerBookingResponse.getConsignee().setGuid(null);
            }
            if (customerBookingResponse.getConsignor() != null) {
                customerBookingResponse.getConsignor().setId(null);
                customerBookingResponse.getConsignor().setGuid(null);
            }
            if (customerBookingResponse.getNotifyParty() != null) {
                customerBookingResponse.getNotifyParty().setId(null);
                customerBookingResponse.getNotifyParty().setGuid(null);
            }
            if (customerBookingResponse.getCarrierDetails() != null) {
                customerBookingResponse.getCarrierDetails().setId(null);
                customerBookingResponse.getCarrierDetails().setGuid(null);
                customerBookingResponse.getCarrierDetails().setCarrierAddedFromNpm(null);
                customerBookingResponse.getCarrierDetails().setVessel(null);
                customerBookingResponse.getCarrierDetails().setVoyage(null);
            }
            if (customerBookingResponse.getContainersList() != null && !customerBookingResponse.getContainersList().isEmpty()) {
                customerBookingResponse.setContainersList(customerBookingResponse.getContainersList().stream().map(containerResponse -> {
                    ContainerResponse c = new ContainerResponse();
                    c.setContainerCode(containerResponse.getContainerCode());
                    c.setCommodityGroup(containerResponse.getCommodityGroup());
                    c.setContainerCount(containerResponse.getContainerCount());
                    c.setGrossWeight(containerResponse.getGrossWeight());
                    c.setGrossWeightUnit(containerResponse.getGrossWeightUnit());
                    return c;
                }).toList());
            }
            if (customerBookingResponse.getPackingList() != null && !customerBookingResponse.getPackingList().isEmpty()) {
                customerBookingResponse.setPackingList(customerBookingResponse.getPackingList().stream().map(packingResponse -> {
                    PackingResponse p = new PackingResponse();
                    p.setPacks(packingResponse.getPacks());
                    p.setPacksType(packingResponse.getPacksType());
                    p.setWeight(packingResponse.getWeight());
                    p.setWeightUnit(packingResponse.getWeightUnit());
                    p.setVolume(packingResponse.getVolume());
                    p.setVolumeUnit(packingResponse.getVolumeUnit());
                    p.setLength(packingResponse.getLength());
                    p.setLengthUnit(packingResponse.getLengthUnit());
                    p.setWidth(packingResponse.getWidth());
                    p.setWidthUnit(packingResponse.getWidthUnit());
                    p.setHeight(packingResponse.getHeight());
                    p.setHeightUnit(packingResponse.getHeightUnit());
                    p.setGoodsDescription(packingResponse.getGoodsDescription());
                    p.setNetWeight(packingResponse.getNetWeight());
                    p.setNetWeightUnit(packingResponse.getNetWeightUnit());
                    p.setVolumeWeight(packingResponse.getVolumeWeight());
                    p.setVolumeWeightUnit(packingResponse.getVolumeWeightUnit());
                    p.setCommodityGroup(packingResponse.getCommodityGroup());
                    p.setChargeable(packingResponse.getChargeable());
                    p.setChargeableUnit(packingResponse.getChargeableUnit());
                    return p;
                }).toList());
            }
            if (customerBookingResponse.getRoutingList() != null && !customerBookingResponse.getRoutingList().isEmpty()) {
                customerBookingResponse.setRoutingList(customerBookingResponse.getRoutingList().stream().map(routingsResponse -> {
                    RoutingsResponse r = new RoutingsResponse();
                    r.setLeg(routingsResponse.getLeg());
                    r.setMode(routingsResponse.getMode());
                    r.setPol(routingsResponse.getPol());
                    r.setPod(routingsResponse.getPod());
                    return r;
                }).toList());
            }
            customerBookingResponse.setBookingCharges(null);

            //fields related to contract
            customerBookingResponse.setContractId(null);
            customerBookingResponse.setParentContractId(null);
            customerBookingResponse.setContractStatus(null);
            customerBookingResponse.setCurrentPartyForQuote(null);
            customerBookingResponse.setBusinessCode(null);

            //fields related to sales branch
            customerBookingResponse.setSalesBranch(null);
            customerBookingResponse.setSecondarySalesAgentEmail(null);
            customerBookingResponse.setPrimarySalesAgentEmail(null);

            //fields related to shipment
            customerBookingResponse.setShipmentId(null);
            customerBookingResponse.setShipmentGuid(null);
            customerBookingResponse.setShipmentEntityIdV2(null);
            customerBookingResponse.setShipmentEntityId(null);
            customerBookingResponse.setShipmentCreatedDate(null);
            customerBookingResponse.setIsBillCreated(null);

            //fields related to order
            customerBookingResponse.setOrderManagementId(null);
            customerBookingResponse.setOrderManagementNumber(null);

            createCustomerBookingResponse(customerBooking.get(), customerBookingResponse);

            return ResponseHelper.buildSuccessResponse(customerBookingResponse);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public boolean checkForCreditLimitManagement(CustomerBooking booking) throws RunnerException {
        ApprovalPartiesRequest approvalPartiesRequest = ApprovalPartiesRequest.builder().build();
        List<Object[]> parties = List.of(
                        new Object[]{booking.getCustomer(), PartyType.CLIENT},
                        new Object[]{booking.getConsignee(), PartyType.CONSIGNEE},
                        new Object[]{booking.getConsignor(), PartyType.CONSIGNOR},
                        new Object[]{booking.getNotifyParty(), PartyType.NOTIFY_PARTY}
                )
                .stream()
                .filter(entry -> entry[0] != null)
                .toList();
        List<ApprovalPartiesRequest.ApprovalParty> partiesList = new ArrayList<>();
        for (Object[] partyEntry : parties) {
            Parties party = (Parties) partyEntry[0];
            if (party == null) continue;
            var orgId = party.getOrgData() != null ? StringUtility.convertToString(party.getOrgData().get(PartiesConstants.ID)) : null;
            var addressId = party.getAddressData() != null ? StringUtility.convertToString(party.getAddressData().get(PartiesConstants.ID)) : null;
            if (StringUtility.isEmpty(orgId) || StringUtility.isEmpty(addressId))
                continue;
            var orgType = partyEntry[1].toString();
            partiesList.add(ApprovalPartiesRequest.ApprovalParty.builder()
                    .addressId(addressId)
                    .orgId(orgId)
                    .entityType(CustomerBookingConstants.CUSTOMER_BOOKING_STRING)
                    .entityId(String.valueOf(booking.getGuid()))
                    .orgType(orgType)
                    .build());
        }
        approvalPartiesRequest.setCreditDetailsRequests(partiesList);
        approvalPartiesRequest.setOperation("CUS_BK");

        String finalStatus = mdmServiceAdapter.getApprovalStausForParties(CommonRequestModel.builder().data(approvalPartiesRequest).build());

        return StringUtils.equals(finalStatus, CustomerBookingConstants.MDM_FINAL_STATUS_APPROVED) || StringUtils.equals(finalStatus, CustomerBookingConstants.MDM_FINAL_STATUS_NO_APPROVAL_NEEDED);
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveByOrderId(String orderId) throws RunnerException {
        try {
            CustomerBookingResponse response = orderManagementAdapter.getOrderForBooking(orderId);
            createCustomerBookingResponse(null, response);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    public void pushCustomerBookingDataToDependentService(CustomerBooking customerBooking, boolean isCreate) {
        try {
            OrderManageDto.OrderManagement orderManagement = OrderManageDto.OrderManagement.builder().orderManagementId(customerBooking.getOrderManagementId()).orderManagementNumber(customerBooking.getOrderManagementNumber()).moduleId(customerBooking.getBookingNumber()).moduleGuid(customerBooking.getGuid().toString()).tenantId(TenantContext.getCurrentTenant()).build();
            KafkaResponse kafkaResponse = producer.getKafkaResponse(orderManagement, isCreate);
            log.info("Producing order management data to kafka with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(kafkaResponse));
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, StringUtility.convertToString(customerBooking.getGuid()));
        } catch (Exception e) {
            log.error("Error Producing Order Management Data to kafka, error is due to " + e.getMessage());
        }
    }

    private boolean checkNPMContractUtilization(CustomerBooking customerBooking) {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        if (Boolean.TRUE.equals(shipmentSettingsDetails.getIsAlwaysUtilization())) {
            return true;
        }
        if (Boolean.TRUE.equals(shipmentSettingsDetails.getIsUtilizationForContainerQuoted())
                && !CommonUtils.listIsNullOrEmpty(customerBooking.getContainersList())) {
            QuoteContracts quoteContracts = quoteContractsService.getQuoteContractsByContractId(customerBooking.getContractId());

            if (quoteContracts == null || CommonUtils.listIsNullOrEmpty(quoteContracts.getContainerTypes())) {
                return false;
            }

            // Check if all containers in booking match the contract's container types
            return areAllContainersQuoted(customerBooking.getContainersList(), quoteContracts.getContainerTypes());
        }

        return false;
    }

    private boolean areAllContainersQuoted(List<Containers> containersList, List<String> containerTypes) {
        for (Containers container : containersList) {
            if (!containerTypes.contains(container.getContainerCode())) {
                return false;
            }
        }
        return true;
    }
}
