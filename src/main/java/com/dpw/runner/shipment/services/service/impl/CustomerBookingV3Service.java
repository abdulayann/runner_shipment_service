package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.INPMServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
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
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingSource;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.entity.enums.PartyType;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.dto.OrderManageDto;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IQuoteContractsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.*;
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
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_EXP;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.validator.constants.CustomerBookingConstants.*;
import static com.dpw.runner.shipment.services.validator.constants.CustomerBookingConstants.NOTIFY_PARTY_REQUEST;

@Service
@Slf4j
public class CustomerBookingV3Service implements ICustomerBookingV3Service {

    ExecutorService executorService = Executors.newFixedThreadPool(10);
    private static final Random rnd = new SecureRandom();

    @Value("${booking.event.kafka.queue}")
    private String senderQueue;

    private final JsonHelper jsonHelper;
    private final IQuoteContractsService quoteContractsService;
    private final INPMServiceAdapter npmService;
    private final CommonUtils commonUtils;
    private final MasterDataUtils masterDataUtils;
    private final BookingIntegrationsUtility bookingIntegrationsUtility;
    private final ICustomerBookingDao customerBookingDao;
    private final IAuditLogService auditLogService;
    private final IPackingDao packingDao;
    private final IContainerDao containerDao;
    private final IReferenceNumbersDao referenceNumbersDao;
    private final IRoutingsDao routingsDao;
    private final IEventDao eventDao;
    private final IBookingChargesDao bookingChargesDao;
    private final IPartiesDao partiesDao;
    private final IShipmentDao shipmentDao;
    private final KafkaProducer producer;
    private final IMDMServiceAdapter mdmServiceAdapter;
    private final IOrderManagementAdapter orderManagementAdapter;
    private final IV1Service v1Service;
    private final ModelMapper modelMapper;
    private final DependentServiceHelper dependentServiceHelper;

    private Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
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
            Map.entry("shipmentCreatedDate", RunnerEntityMapping.builder().tableName(Constants.CUSTOMER_BOOKING).dataType(LocalDateTime.class).fieldName("shipmentCreatedDate").build()),
            Map.entry("source", RunnerEntityMapping.builder().tableName(Constants.CUSTOMER_BOOKING).dataType(BookingSource.class).fieldName("source").build()),
            Map.entry("shipmentReferenceNumber", RunnerEntityMapping.builder().tableName(Constants.CUSTOMER_BOOKING).dataType(String.class).fieldName("shipmentReferenceNumber").build())
    );

    @Autowired
    public CustomerBookingV3Service(JsonHelper jsonHelper,
                                    IQuoteContractsService quoteContractsService,
                                    INPMServiceAdapter npmService,
                                    CommonUtils commonUtils,
                                    MasterDataUtils masterDataUtils,
                                    BookingIntegrationsUtility bookingIntegrationsUtility,
                                    ICustomerBookingDao customerBookingDao,
                                    IAuditLogService auditLogService,
                                    IPackingDao packingDao,
                                    IContainerDao containerDao,
                                    IReferenceNumbersDao referenceNumbersDao,
                                    IRoutingsDao routingsDao,
                                    IEventDao eventDao,
                                    IBookingChargesDao bookingChargesDao,
                                    KafkaProducer producer,
                                    IMDMServiceAdapter mdmServiceAdapter,
                                    IOrderManagementAdapter orderManagementAdapter,
                                    IPartiesDao partiesDao,
                                    IShipmentDao shipmentDao,
                                    IV1Service v1Service,
                                    ModelMapper modelMapper,
                                    DependentServiceHelper dependentServiceHelper){
        this.jsonHelper = jsonHelper;
        this.quoteContractsService = quoteContractsService;
        this.npmService = npmService;
        this.commonUtils = commonUtils;
        this.masterDataUtils = masterDataUtils;
        this.bookingIntegrationsUtility = bookingIntegrationsUtility;
        this.customerBookingDao = customerBookingDao;
        this.auditLogService = auditLogService;
        this.packingDao = packingDao;
        this.containerDao = containerDao;
        this.referenceNumbersDao = referenceNumbersDao;
        this.routingsDao = routingsDao;
        this.eventDao = eventDao;
        this.bookingChargesDao = bookingChargesDao;
        this.producer = producer;
        this.mdmServiceAdapter = mdmServiceAdapter;
        this.orderManagementAdapter = orderManagementAdapter;
        this.partiesDao = partiesDao;
        this.shipmentDao = shipmentDao;
        this.v1Service = v1Service;
        this.modelMapper = modelMapper;
        this.dependentServiceHelper = dependentServiceHelper;
    }

    @Override
    public CustomerBookingV3Response create(CustomerBookingV3Request customerBookingV3Request) throws RunnerException {
        if (customerBookingV3Request == null) {
            log.error("Request is null for Customer Booking Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }
        CustomerBooking customerBooking = jsonHelper.convertValue(customerBookingV3Request, CustomerBooking.class);
        customerBooking.setSource(BookingSource.Runner);
        // Update NPM for contract utilization
        if(checkNPMContractUtilization(customerBooking)) {
            npmContractUpdate(customerBooking, null, false, CustomerBookingConstants.REMOVE, false);
        }
        try {
            createEntities(customerBooking, customerBookingV3Request);
            /**
             * Platform service integration
             * Criteria for update call to platform service : check flag IsPlatformBookingCreated, if true then update otherwise dont update
             */
            //Check 1
            V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
            if (Objects.equals(customerBooking.getBookingStatus(), BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                    && (Boolean.FALSE.equals(v1TenantSettingsResponse.getFetchRatesMandate()) || (!Objects.isNull(customerBooking.getBookingCharges()) && !customerBooking.getBookingCharges().isEmpty()))) {
                // Triggering Event for customer booking for DependentServices update
                triggerPushToDownStreamForCustomerBooking(customerBooking);
            }
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RunnerException(e.getMessage());
        }
        return jsonHelper.convertValue(customerBooking, CustomerBookingV3Response.class);
    }

    private void triggerPushToDownStreamForCustomerBooking(CustomerBooking customerBooking) {
        Long bookingId = customerBooking.getId();
        String transactionId = bookingId.toString();

        log.info("[InternalKafkaPush] Initiating downstream internal Kafka push | bookingId={} | transactionId={}",
                bookingId, transactionId);

        PushToDownstreamEventDto pushToDownstreamEventDto = PushToDownstreamEventDto.builder()
                .parentEntityId(bookingId)
                .parentEntityName(Constants.CUSTOMER_BOOKING)
                .meta(PushToDownstreamEventDto.Meta.builder()
                        .tenantId(customerBooking.getTenantId())
                        .isCreate(true).build())
                .build();
        dependentServiceHelper.pushToKafkaForDownStream(pushToDownstreamEventDto, transactionId);

        log.info("[InternalKafkaPush] Message successfully pushed to internal Kafka | bookingId={} | transactionId={}",
               bookingId, bookingId);
    }

    @Override
    public CustomerBookingV3Response update(CustomerBookingV3Request request) throws RunnerException {
        if (request == null || request.getId() == null) {
            log.error("Request is empty for Booking update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
        }
        Long id = request.getId();
        Optional<CustomerBooking> oldEntity = customerBookingDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug(CustomerBookingConstants.BOOKING_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        boolean eventPersisted = false;
        Optional<Events> persistedEvent = eventDao.findByEntityIdAndEntityType(oldEntity.get().getId(), Constants.BOOKING);
        if(persistedEvent.isPresent())
            eventPersisted = true;
        if(!eventPersisted && request.getBookingStatus().equals(BookingStatus.PENDING_FOR_KYC) && oldEntity.get().getBookingStatus().equals(BookingStatus.PENDING_FOR_REVIEW)) {
            createAutomatedEvents(request, EventConstants.BKCR, LocalDateTime.now(), null, null);
        }

        if (Objects.equals(oldEntity.get().getBookingStatus(), BookingStatus.READY_FOR_SHIPMENT)) {
            throw new ValidationException("Booking alterations are not allowed once booking moved to Ready For Shipment.");
        }
        boolean isCreatedInPlatform = !Objects.isNull(oldEntity.get().getIsPlatformBookingCreated()) && oldEntity.get().getIsPlatformBookingCreated();
        //todo: manual + existing mapper
        CustomerBooking customerBooking = jsonHelper.convertValue(request, CustomerBooking.class);
        customerBooking.setCreatedAt(oldEntity.get().getCreatedAt());
        customerBooking.setCreatedBy(oldEntity.get().getCreatedBy());
        customerBooking.setIsPlatformBookingCreated(isCreatedInPlatform);
        customerBooking.setSource(oldEntity.get().getSource());

        // NPM update contract
        if(checkNPMContractUtilization(customerBooking)) {
            contractUtilisationForUpdate(customerBooking, oldEntity.get());
        }
        customerBooking = this.updateEntities(customerBooking, request, jsonHelper.convertToJson(oldEntity.get()));
        try {
            //Check 2
            V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
            if (!Objects.equals(customerBooking.getBookingStatus(), BookingStatus.PENDING_FOR_KYC)
                    && (Boolean.FALSE.equals(v1TenantSettingsResponse.getFetchRatesMandate()) || (!Objects.isNull(customerBooking.getBookingCharges()) && !customerBooking.getBookingCharges().isEmpty()))) {

                // Triggering Event for customer booking for DependentServices update
                triggerPushToDownStreamForCustomerBooking(customerBooking);
            }
        } catch (Exception e) {
            log.error(e.getMessage());
        }

        return jsonHelper.convertValue(customerBooking, CustomerBookingV3Response.class);
    }

    @Override
    public CustomerBookingV3DeleteResponse delete(Long bookingId) throws RunnerException {
        String responseMsg;
        try {
            if (bookingId == null) {
                log.debug("BookingId is null for Booking delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Optional<CustomerBooking> customerBooking = customerBookingDao.findById(bookingId);
            if (!customerBooking.isPresent()) {
                log.debug(CustomerBookingConstants.BOOKING_DETAILS_RETRIEVE_BY_ID_ERROR, bookingId, LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            customerBookingDao.delete(customerBooking.get());
            log.info("Deleted Booking details for Id {} with Request Id {}", bookingId, LoggerHelper.getRequestIdFromMDC());
            return CustomerBookingV3DeleteResponse.builder().message(String.format("Booking %s deleted successfully!", bookingId)).build();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(String.format("Failed to delete Booking %s !", bookingId));
        }
    }

    @Override
    public CustomerBookingV3ListResponse list(ListCommonRequest request) throws RunnerException {
        String responseMsg;
        try {
            if (request == null) {
                log.error("Request is empty for Booking list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Pair<Specification<CustomerBooking>, Pageable> tuple = fetchData(request, CustomerBooking.class, tableNames);
            Page<CustomerBooking> customerBookingPage = customerBookingDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Booking list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CustomerBookingV3ListResponse.builder()
                    .customerBookingV3Responses(jsonHelper.convertValueToList(customerBookingPage.getContent(),CustomerBookingV3Response.class))
                    .totalPages(customerBookingPage.getTotalPages())
                    .totalCount(customerBookingPage.getTotalElements())
                    .build();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    @Override
    public CustomerBookingV3Response retrieveById(CommonGetRequest request) throws RunnerException {
        String responseMsg;
        try {
            double startTime = System.currentTimeMillis();
            if (request == null || (request.getId() == null && request.getGuid() == null)) {
                log.error("Request Id and Guid are null for Booking retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_INVALID_REQUEST_MSG);
            }
            Long id = request.getId();
            Optional<CustomerBooking> customerBooking;
            if(id != null) {
                customerBooking = customerBookingDao.findById(id);
            } else {
                UUID guid = UUID.fromString(request.getGuid());
                customerBooking = customerBookingDao.findByGuid(guid);
            }

            if (!customerBooking.isPresent()) {
                log.debug(CustomerBookingConstants.BOOKING_DETAILS_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            //Fetch all parties based on entity type and entity id and exclude certain parties and keep remaining ones as additional parties
            List<Parties> allParties = partiesDao.findByEntityIdAndEntityType(customerBooking.get().getId(), "CUSTOMER_BOOKING");
            List<String> excludedTypes = Arrays.asList("CLIENT", "CONSIGNOR", "CONSIGNEE", "NOTIFY PARTY");
            List<Parties> additionalParties = allParties.stream().filter(party -> !excludedTypes.contains(party.getType())).toList();
            customerBooking.get().setAdditionalParties(additionalParties);

            double current = System.currentTimeMillis();
            log.info("Booking details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            log.info("Time taken to fetch booking details from db: {} Request Id {}", current - startTime, LoggerHelper.getRequestIdFromMDC());
            CustomerBookingV3Response customerBookingResponse = jsonHelper.convertValue(customerBooking.get(), CustomerBookingV3Response.class);
            double nextTime = System.currentTimeMillis();
            log.info("Time taken to fetch details from db: {} Request Id {}", nextTime - current, LoggerHelper.getRequestIdFromMDC());
            createCustomerBookingResponse(customerBooking.get(), customerBookingResponse);
            return customerBookingResponse;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    @Override
    public CustomerBookingV3Response cloneBooking(Long id) throws RunnerException {
        if(Objects.isNull(id)) {
            log.error("Request Id is null for booking cloning with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("Booking Id cannot be null");
        }
        String responseMsg;
        try {
            Optional<CustomerBooking> customerBooking = getValidatedCustomerBooking(id);
            CustomerBookingV3Response customerBookingResponse = jsonHelper.convertValue(customerBooking.get(), CustomerBookingV3Response.class);
            customerBookingResponse.setId(null);
            customerBookingResponse.setGuid(null);
            customerBookingResponse.setBookingNumber(null);
            customerBookingResponse.setBookingStatus(BookingStatus.PENDING_FOR_KYC);
            customerBookingResponse.setSource(null);
            customerBookingResponse.setCreatedBy(null);
            customerBookingResponse.setSourceGuid(customerBooking.get().getGuid());
            customerBookingResponse.setBookingDate(LocalDateTime.now());
            if(customerBookingResponse.getCustomer() != null)
            {
                customerBookingResponse.getCustomer().setId(null);
                customerBookingResponse.getCustomer().setGuid(null);
            }
            if(customerBookingResponse.getConsignee() != null)
            {
                customerBookingResponse.getConsignee().setId(null);
                customerBookingResponse.getConsignee().setGuid(null);
            }
            if(customerBookingResponse.getConsignor() != null)
            {
                customerBookingResponse.getConsignor().setId(null);
                customerBookingResponse.getConsignor().setGuid(null);
            }
            if(customerBookingResponse.getNotifyParty() != null)
            {
                customerBookingResponse.getNotifyParty().setId(null);
                customerBookingResponse.getNotifyParty().setGuid(null);
            }
            if(customerBookingResponse.getCarrierDetails() != null)
            {
                customerBookingResponse.getCarrierDetails().setId(null);
                customerBookingResponse.getCarrierDetails().setGuid(null);
                customerBookingResponse.getCarrierDetails().setCarrierAddedFromNpm(null);
                customerBookingResponse.getCarrierDetails().setVessel(null);
                customerBookingResponse.getCarrierDetails().setVoyage(null);
            }
            if(customerBookingResponse.getContainersList() != null && !customerBookingResponse.getContainersList().isEmpty())
            {
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
            if(customerBookingResponse.getPackingList() != null && !customerBookingResponse.getPackingList().isEmpty())
            {
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
            if(customerBookingResponse.getRoutingList() != null && !customerBookingResponse.getRoutingList().isEmpty())
            {
                customerBookingResponse.setRoutingList(customerBookingResponse.getRoutingList().stream().map(routingsResponse -> {
                    RoutingsResponse r = new RoutingsResponse();
                    r.setLeg(routingsResponse.getLeg());
                    r.setMode(routingsResponse.getMode());
                    r.setPol(routingsResponse.getPol());
                    r.setPod(routingsResponse.getPod());
                    return r;
                }).toList());
            }
            setReferenceNumbersForClonedBookings(customerBookingResponse);
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

            return customerBookingResponse;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    @Override
    public CustomerBookingV3Response retrieveByOrderId(String orderId) throws RunnerException {
        try {
            CustomerBookingV3Response response = orderManagementAdapter.getOrderForBookingV3(orderId);
            createCustomerBookingResponse(null, response);
            return response;
        } catch (Exception e){
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public PlatformToRunnerCustomerBookingResponse platformCreateBooking(PlatformToRunnerCustomerBookingRequest request) throws RunnerException {
        if (request.getIsSingleUsageContract() != null)
            request.setContractStatus(Boolean.TRUE.equals(request.getIsSingleUsageContract()) ? "SINGLE_USAGE" : "MULTI_USAGE");
        String bookingNumber = request.getBookingNumber();
        if (bookingNumber == null) {
            log.error("Booking Number is empty for create Booking with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Optional<CustomerBooking> customerBooking = findCustomerBooking(request);

        PlatformToRunnerCustomerBookingResponse platformResponse = new PlatformToRunnerCustomerBookingResponse();
        platformResponse.setBookingNumber(bookingNumber);
        setOrgAndAddressToParties(request);

        processRequestListsForResponse(request, platformResponse);

        if (request.getBookingCharges() != null) {
            List<PlatformToRunnerCustomerBookingResponse.ReferenceNumbersGuidMapResponse> referenceNumbersGuidMapResponses = new ArrayList<>();
            request.getBookingCharges().forEach(charge -> {
                if (charge.getGuid() == null)
                    charge.setGuid(UUID.randomUUID());
                referenceNumbersGuidMapResponses.add(PlatformToRunnerCustomerBookingResponse.ReferenceNumbersGuidMapResponse.builder()
                        .reference_id(charge.getReference_id())
                        .guid(charge.getGuid())
                        .build());

            });
            platformResponse.setCharges(referenceNumbersGuidMapResponses);
        }
        if(request.getBookingStatus()==null && request.getSource()!=null && BookingSource.B2B.equals(request.getSource()))
            request.setBookingStatus(BookingStatus.PENDING_FOR_REVIEW);

        CustomerBookingV3Request customerBookingRequest = modelMapper.map(request, CustomerBookingV3Request.class);
        assignCarrierDetailsToRequest(customerBookingRequest, request);
        if (customerBooking.isEmpty()) {
            customerBookingRequest.setCurrentPartyForQuote("CLIENT");
            this.createPlatformBooking(customerBookingRequest);
        } else {
            updateDataInExistingBooking(customerBooking.get(), customerBookingRequest);

            this.updatePlatformBooking(customerBookingRequest, customerBooking.get());
        }

        return platformResponse;
    }

    private void updateDataInExistingBooking(CustomerBooking customerBooking, CustomerBookingV3Request customerBookingRequest) {
        if (Objects.equals(customerBooking.getBookingStatus(), BookingStatus.READY_FOR_SHIPMENT))
            throw new ValidationException("Booking alterations are not allowed once booking moved to Ready For Shipment.");
        if (customerBooking.getId() != null)
            customerBookingRequest.setId(customerBooking.getId());
        if (customerBooking.getGuid() != null)
            customerBookingRequest.setGuid(customerBooking.getGuid());
        if (customerBooking.getCarrierDetails() != null) {
            customerBookingRequest.getCarrierDetails().setId(customerBooking.getCarrierDetails().getId());
            customerBookingRequest.getCarrierDetails().setGuid(customerBooking.getCarrierDetails().getGuid());
        }
        if(customerBooking.getCurrentPartyForQuote() != null) {
            customerBookingRequest.setCurrentPartyForQuote(customerBooking.getCurrentPartyForQuote());
        }

        Map<UUID, Long> guidVsIdContainerMap = new HashMap<>();
        Map<UUID, Long> guidVsIdPackingMap = new HashMap<>();
        Map<UUID, Long> guidVsIdRoutingMap = new HashMap<>();
        Map<UUID, Long> guidVsIdChargesMap = new HashMap<>();
        if (customerBooking.getContainersList() != null) {
            customerBooking.getContainersList().forEach(cont -> guidVsIdContainerMap.put(cont.getGuid(), cont.getId()));
        }

        if (customerBooking.getPackingList() != null) {
            customerBooking.getPackingList().forEach(pack -> guidVsIdPackingMap.put(pack.getGuid(), pack.getId()));
        }

        if (customerBooking.getRoutingList() != null) {
            customerBooking.getRoutingList().forEach(route -> guidVsIdRoutingMap.put(route.getGuid(), route.getId()));
        }

        if (customerBooking.getBookingCharges() != null) {
            customerBooking.getBookingCharges().forEach(charge -> guidVsIdChargesMap.put(charge.getGuid(), charge.getId()));
        }

        setCountWithContainersList(customerBookingRequest, guidVsIdContainerMap);

        setPackWithPackingsList(customerBookingRequest, guidVsIdPackingMap);

        setRouteWithRoutingList(customerBookingRequest, guidVsIdRoutingMap);

        setChargeWithBookingCharges(customerBookingRequest, guidVsIdChargesMap);
    }

    private void setChargeWithBookingCharges(CustomerBookingV3Request customerBookingRequest, Map<UUID, Long> guidVsIdChargesMap) {
        if (customerBookingRequest.getBookingCharges() != null) {
            customerBookingRequest.getBookingCharges().forEach(charge -> {
                if (charge.getGuid() != null && guidVsIdChargesMap.containsKey(charge.getGuid()))
                    charge.setId(guidVsIdChargesMap.get(charge.getGuid()));

            });
        }
    }

    private void setRouteWithRoutingList(CustomerBookingV3Request customerBookingRequest, Map<UUID, Long> guidVsIdRoutingMap) {
        if (customerBookingRequest.getRoutingList() != null) {
            customerBookingRequest.getRoutingList().forEach(route -> {
                if (route.getGuid() != null && guidVsIdRoutingMap.containsKey(route.getGuid()))
                    route.setId(guidVsIdRoutingMap.get(route.getGuid()));

            });
        }
    }

    private void setPackWithPackingsList(CustomerBookingV3Request customerBookingRequest, Map<UUID, Long> guidVsIdPackingMap) {
        if (customerBookingRequest.getPackingList() != null) {
            customerBookingRequest.getPackingList().forEach(pack -> {
                if (pack.getGuid() != null && guidVsIdPackingMap.containsKey(pack.getGuid()))
                    pack.setId(guidVsIdPackingMap.get(pack.getGuid()));

            });
        }
    }

    private void setCountWithContainersList(CustomerBookingV3Request customerBookingRequest, Map<UUID, Long> guidVsIdContainerMap) {
        if (customerBookingRequest.getContainersList() != null) {
            customerBookingRequest.getContainersList().forEach(cont -> {
                if (cont.getGuid() != null && guidVsIdContainerMap.containsKey(cont.getGuid()))
                    cont.setId(guidVsIdContainerMap.get(cont.getGuid()));

            });
        }
    }

    private CustomerBookingV3Response updatePlatformBooking(CustomerBookingV3Request request, CustomerBooking oldEntity) throws RunnerException {
        CustomerBooking customerBooking = jsonHelper.convertValue(request, CustomerBooking.class);
        customerBooking.setIsPlatformBookingCreated(Boolean.TRUE);
        if(request.getSource()==null)
            customerBooking.setSource(BookingSource.Platform);
        try {
            customerBooking = this.updateEntities(customerBooking, request, jsonHelper.convertToJson(oldEntity));
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RunnerException(e.getMessage());
        }
        return jsonHelper.convertValue(customerBooking, CustomerBookingV3Response.class);
    }

    private CustomerBookingV3Response createPlatformBooking(CustomerBookingV3Request request) throws RunnerException {
        if (request == null) {
            log.error("Request is null for Customer Booking Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        CustomerBooking customerBooking = jsonHelper.convertValue(request, CustomerBooking.class);
        customerBooking.setIsConsigneeAddressFreeText(customerBooking.getIsConsigneeFreeText() != null && customerBooking.getIsConsigneeFreeText());
        customerBooking.setIsConsignorAddressFreeText(customerBooking.getIsConsignorFreeText() != null && customerBooking.getIsConsignorFreeText());
        customerBooking.setIsCustomerAddressFreeText(false);
        customerBooking.setIsNotifyPartyAddressFreeText(customerBooking.getIsNotifyPartyFreeText() != null && customerBooking.getIsNotifyPartyFreeText());
        if(request!=null && request.getSource()==null)
            customerBooking.setSource(BookingSource.Platform);
        customerBooking.setIsPlatformBookingCreated(Boolean.TRUE);
        try {
            createEntities(customerBooking, request);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RunnerException(e.getMessage());
        }
        return jsonHelper.convertValue(customerBooking, CustomerBookingV3Response.class);
    }

    private void assignCarrierDetailsToRequest(CustomerBookingV3Request customerBookingRequest, PlatformToRunnerCustomerBookingRequest request) {

        String vessel = null;
        if(!isStringNullOrEmpty(request.getVessel())) {
            VesselsResponse vesselsResponse = getVesselsData(request.getVessel());
            if(vesselsResponse != null)
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
        if(isStringNullOrEmpty(carrierSCACCode))
            return null;
        List<String> carrierCodes = new ArrayList<>();
        carrierCodes.add(carrierSCACCode);
        Map<String, EntityTransferCarrier> map = masterDataUtils.fetchInBulkCarriersBySCACCode(carrierCodes);
        if(map.containsKey(carrierSCACCode))
            return map.get(carrierSCACCode).ItemValue;
        return null;
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
        if(vesselsResponse != null && !vesselsResponse.isEmpty())
            return vesselsResponse.get(0);
        return null;
    }

    private void processRequestListsForResponse(PlatformToRunnerCustomerBookingRequest request, PlatformToRunnerCustomerBookingResponse platformResponse) {
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
    }

    private void setOrgAndAddressToParties(PlatformToRunnerCustomerBookingRequest request) {
        Map<String, PartiesRequest> requestMap = new HashMap<>();
        if (request.getCustomer() != null) {
            requestMap.put(CUSTOMER_REQUEST, request.getCustomer());
        }
        if(request.getConsignor() != null && !Boolean.TRUE.equals(request.getIsConsignorFreeText()) &&
                !isStringNullOrEmpty(request.getConsignor().getOrgCode()) &&
                !isStringNullOrEmpty(request.getConsignor().getAddressCode())) {
            requestMap.put(CONSIGNOR_REQUEST, request.getConsignor());
        }
        else {
            transformOrgAndAddressToRawData(request.getConsignor());
        }
        if(request.getConsignee() != null && !Boolean.TRUE.equals(request.getIsConsigneeFreeText()) &&
                !isStringNullOrEmpty(request.getConsignee().getOrgCode()) &&
                !isStringNullOrEmpty(request.getConsignee().getAddressCode())) {
            requestMap.put(CONSIGNEE_REQUEST, request.getConsignee());
        }
        else {
            transformOrgAndAddressToRawData(request.getConsignee());
        }
        if(request.getNotifyParty() != null && !Boolean.TRUE.equals(request.getIsNotifyPartyFreeText()) &&
                !isStringNullOrEmpty(request.getNotifyParty().getOrgCode()) &&
                !isStringNullOrEmpty(request.getNotifyParty().getAddressCode())) {
            requestMap.put(NOTIFY_PARTY_REQUEST, request.getNotifyParty());
        }
        else {
            transformOrgAndAddressToRawData(request.getNotifyParty());
        }
        bookingIntegrationsUtility.transformOrgAndAddressPayloadToGivenParties(requestMap);
        if(requestMap.containsKey("Customer"))
            request.setCustomer(requestMap.get("Customer"));
        if(requestMap.containsKey("Consignor"))
            request.setConsignor(requestMap.get("Consignor"));
        if(requestMap.containsKey("Consignee"))
            request.setConsignee(requestMap.get("Consignee"));
        if(requestMap.containsKey("Notify Party"))
            request.setNotifyParty(requestMap.get("Notify Party"));

        processRequestBookingCharges(request);
    }

    private void processRequestBookingCharges(PlatformToRunnerCustomerBookingRequest request) {
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
        if(partiesRequest == null)
            return;
        Map<String, Object> orgData = partiesRequest.getOrgData();
        Map<String, Object> addressData = partiesRequest.getAddressData();

        String orgString = "";
        String addressString = "";
        if (orgData.containsKey(PartiesConstants.FULLNAME)) {
            orgString = orgString.concat((String) orgData.get(PartiesConstants.FULLNAME));
            addressString = addressString.concat((String) orgData.get(PartiesConstants.FULLNAME) + "|");
        }
        partiesRequest.setOrgData(new HashMap<>(Map.of(PartiesConstants.RAW_DATA, orgString)));
        partiesRequest.getOrgData().putAll(orgData);
        if (addressData.containsKey(PartiesConstants.ADDRESS1)) {
            addressString = addressString.concat((String) addressData.get(PartiesConstants.ADDRESS1) + "|");
        }
        if (addressData.containsKey(PartiesConstants.ADDRESS2)) {
            addressString = addressString.concat((String) addressData.get(PartiesConstants.ADDRESS2) + "|");
        }
        if (addressData.containsKey(PartiesConstants.CITY)) {
            addressString = addressString.concat((String) addressData.get(PartiesConstants.CITY) + "|");
        }
        if (addressData.containsKey(PartiesConstants.STATE)) {
            addressString = addressString.concat((String) addressData.get(PartiesConstants.STATE) + "|");
        }
        if (addressData.containsKey(PartiesConstants.COUNTRY)) {
            addressString = addressString.concat((String) addressData.get(PartiesConstants.COUNTRY) + "|");
        }
        if (addressData.containsKey(PartiesConstants.ZIP_POST_CODE)) {
            addressString = addressString.concat((String) addressData.get(PartiesConstants.ZIP_POST_CODE) + "|");
        }
        if (addressData.containsKey(PartiesConstants.MOBILE)) {
            addressString = addressString.concat((String) addressData.get(PartiesConstants.MOBILE) + "|");
        }
        if (addressData.containsKey(PartiesConstants.PHONE)) {
            addressString = addressString.concat((String) addressData.get(PartiesConstants.PHONE) + "|");
        }
        partiesRequest.setIsAddressFreeText(true);
        partiesRequest.setAddressData(new HashMap<>(Map.of(PartiesConstants.RAW_DATA, addressString)));
        partiesRequest.getAddressData().putAll(addressData);
    }

    private Optional<CustomerBooking> findCustomerBooking(PlatformToRunnerCustomerBookingRequest request) {
        String bookingNumber = request.getBookingNumber();
        String shipmentReferenceNumber = request.getShipmentReferenceNumber();
        Optional<CustomerBooking> optional;
        if (Constants.TESLA.equalsIgnoreCase(request.getIntegrationSource())) {
            optional = customerBookingDao.findByShipmentReferenceNumber(shipmentReferenceNumber);
            request.setBookingNumber(optional.map(CustomerBooking::getBookingNumber).orElse(null));
        }
        else {
            optional = customerBookingDao.findByBookingNumber(bookingNumber);
        }

        return optional;
    }

    private Optional<CustomerBooking> getValidatedCustomerBooking(Long id) {
        Optional<CustomerBooking> customerBooking = customerBookingDao.findById(id);
        if(customerBooking.isEmpty())
        {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        Boolean countryAirCargoSecurity = shipmentSettingsDetails.getCountryAirCargoSecurity();
        if (Boolean.TRUE.equals(countryAirCargoSecurity) && !CommonUtils.checkAirSecurityForBooking(customerBooking.get())) {
            throw new ValidationException(Constants.AIR_SECURITY_PERMISSION_MSG);
        }
        return customerBooking;
    }

    private void setReferenceNumbersForClonedBookings(CustomerBookingV3Response customerBookingResponse) {
        if(customerBookingResponse.getReferenceNumbersList() != null && !customerBookingResponse.getReferenceNumbersList().isEmpty())
        {
            customerBookingResponse.setReferenceNumbersList(customerBookingResponse.getReferenceNumbersList().stream().map(referenceNumbersResponse -> {
                ReferenceNumbersResponse r = new ReferenceNumbersResponse();
                r.setCountryOfIssue(referenceNumbersResponse.getCountryOfIssue());
                r.setType(referenceNumbersResponse.getType());
                r.setReferenceNumber(referenceNumbersResponse.getReferenceNumber());
                return r;
            }).toList());
        }
    }

    private void createEntities(CustomerBooking customerBooking, CustomerBookingV3Request request) throws RunnerException {
        if (customerBooking.getIsPlatformBookingCreated() == null) {
            customerBooking.setIsPlatformBookingCreated(false);
        }
        if (customerBooking.getBookingNumber() == null) {
            customerBooking.setBookingNumber(generateBookingNumber(customerBooking.getCargoType()));
        }
        if(request.getOrderManagementId() != null) {
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
        request.setId(bookingId);

        saveChildEntities(customerBooking, request);
        generateBookingAcknowledgementEvent(request);

        List<Containers> containers = customerBooking.getContainersList();
        Map<UUID, Containers> containerMap = new HashMap<>();
        if (containers != null && !containers.isEmpty()) {
            for (Containers container : containers) {
                containerMap.put(container.getGuid(), container);
            }
        }
        List<BookingChargesRequest> bookingChargesRequest = request.getBookingCharges();
        processBookingChargesRequest(customerBooking, bookingChargesRequest, containerMap);
        if(request.getOrderManagementId() != null)
        {
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
                            .isIntegrationLog(Objects.equals(customerBooking.getBookingStatus(), BookingStatus.PENDING_FOR_REVIEW))
                            .flow("Inbound")
                            .dataType("Transactional")
                            .operation(DBOperationType.CREATE.name()).build()
            );
        } catch (Exception e) {
            log.error(e.getMessage());
        }
    }

    private void npmContractUpdate(CustomerBooking current, CustomerBooking old, Boolean isAlteration, String operation, boolean isCancelled) {
        if (Objects.equals(current.getTransportType(), Constants.TRANSPORT_MODE_SEA) && !Objects.isNull(current.getContractId()) ) {
            List<LoadInfoRequest> loadInfoRequestList = containersListForLoad(current, old, operation);

            if (!loadInfoRequestList.isEmpty() || !Boolean.TRUE.equals(isAlteration)) {
                String contractStatus = null;
                if (Objects.equals(current.getContractStatus(), CustomerBookingConstants.SINGLE_USAGE) && isCancelled)
                    contractStatus = CustomerBookingConstants.ENABLED;
                else if (Objects.equals(current.getContractStatus(), CustomerBookingConstants.SINGLE_USAGE) )
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

    private void generateBookingAcknowledgementEvent(CustomerBookingV3Request request) {
        // create booking acknowledged event for Tesla
        if(Constants.TESLA.equalsIgnoreCase(request.getIntegrationSource()))
            createAutomatedEvents(request, EventConstants.BKAC, LocalDateTime.now(), null, EventConstants.BKAC_DESCRIPTION);
    }

    private Events createAutomatedEvents(CustomerBookingV3Request request, String eventCode,
                                         LocalDateTime actualDateTime, LocalDateTime estimatedDateTime, String description) {
        Events events = initializeAutomatedEvents(request, eventCode, actualDateTime, estimatedDateTime);
        events.setDescription(description);
        commonUtils.updateEventWithMasterData(List.of(events));
        // Persist the event
        eventDao.save(events);
        return events;
    }

    private Events initializeAutomatedEvents(CustomerBookingV3Request request, String eventCode,
                                             LocalDateTime actualDateTime, LocalDateTime estimatedDateTime) {
        Events events = new Events();
        // Set event fields from booking request
        events.setActual(actualDateTime);
        events.setEstimated(estimatedDateTime);
        events.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
        events.setIsPublicTrackingEvent(true);
        events.setEntityType(Constants.BOOKING);
        events.setEntityId(request.getId());
        events.setTenantId(TenantContext.getCurrentTenant());
        events.setEventCode(eventCode);

        return events;
    }

    private boolean checkNPMContractUtilization(CustomerBooking customerBooking) {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        if(Boolean.TRUE.equals(shipmentSettingsDetails.getIsAlwaysUtilization())) {
            return true;
        }
        if(Boolean.TRUE.equals(shipmentSettingsDetails.getIsUtilizationForContainerQuoted())
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

    private List<LoadInfoRequest> containersListForLoad(CustomerBooking current, CustomerBooking old, String operation) {
        Map<String, Containers> idVsContainerMap = new HashMap<>();
        List<LoadInfoRequest> loadInfoRequestList = new ArrayList<>();

        if (!Objects.isNull(old) && !Objects.isNull(old.getContainersList()))
            idVsContainerMap = old.getContainersList().stream().collect(Collectors.toMap(x -> x.getId() + "-" + x.getContainerCode() + "-" + x.getCommodityGroup(), x -> x));

        if (idVsContainerMap.isEmpty()) {
            // Only current operation, no comparison
            if(current.getContainersList() != null)
            {
                current.getContainersList().forEach(cont ->
                        loadInfoRequestList.add(containerLoadConstruct(cont, operation, cont.getContainerCount()))
                );
            }
        }  else {
            // Find delta
            updateFinalIdVsContainerMap(current, idVsContainerMap, loadInfoRequestList);
        }
        return loadInfoRequestList;
    }

    private boolean areAllContainersQuoted(List<Containers> containersList, List<String> containerTypes) {
        for (Containers container : containersList) {
            if (!containerTypes.contains(container.getContainerCode())) {
                return false;
            }
        }
        return true;
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

    private void updateFinalIdVsContainerMap(CustomerBooking current, Map<String, Containers> finalIdVsContainerMap, List<LoadInfoRequest> loadInfoRequestList) {
        if(current.getContainersList() != null)
        {
            current.getContainersList().forEach(cont -> {
                String key = cont.getId() + "-" + cont.getContainerCode() + "-" + cont.getCommodityGroup();
                if (finalIdVsContainerMap.containsKey(key)) {
                    // existing container with probably quantity change
                    if (!Objects.equals(finalIdVsContainerMap.get(key).getContainerCount(), cont.getContainerCount())) {
                        long diff = cont.getContainerCount() - finalIdVsContainerMap.get(key).getContainerCount();
                        loadInfoRequestList.add(containerLoadConstruct(cont, diff > 0 ? CustomerBookingConstants.REMOVE : CustomerBookingConstants.ADD, Math.abs(diff)));
                    }
                    finalIdVsContainerMap.remove(key);
                }
                else {
                    // New Container
                    loadInfoRequestList.add(containerLoadConstruct(cont, CustomerBookingConstants.REMOVE, cont.getContainerCount()));
                }
            });
        }
        // Release all the remaining loads
        finalIdVsContainerMap.forEach((k, v) -> loadInfoRequestList.add(containerLoadConstruct(v, CustomerBookingConstants.ADD, v.getContainerCount())));
    }

    private String generateBookingNumber(String cargoType) {
        String prefix = "DBAR";
        if (Objects.equals(cargoType, "FCL") || Objects.equals(cargoType, "FTL"))
            prefix = "DBFC";
        else if (Objects.equals(cargoType, "BBK") || Objects.equals(cargoType, "ROR") || Objects.equals(cargoType, "LCL") || Objects.equals(cargoType, "LTL"))
            prefix = "DBLC";
        return prefix + "-" + getRandomNumberString(7) + "-" + getRandomNumberString(6);
    }

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

    private void populateTotalRevenueDetails(CustomerBooking customerBooking, CustomerBookingV3Request request) {
        BigDecimal totalRevenue = BigDecimal.ZERO;
        if (request != null && request.getBookingCharges() != null) {
            totalRevenue = request.getBookingCharges().stream()
                    .filter(Objects::nonNull)
                    .map(c -> c.getLocalSellAmount() != null ? c.getLocalSellAmount() : BigDecimal.ZERO)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
        }
        customerBooking.setTotalRevenue(totalRevenue);
    }

    private void saveChildEntities(CustomerBooking customerBooking, CustomerBookingV3Request request) throws RunnerException {
        Long bookingId = customerBooking.getId();
        List<PackingV3Request> packingRequest = request.getPackingList();
        if (packingRequest != null)
            customerBooking.setPackingList(packingDao.saveEntityFromBooking(commonUtils.convertToEntityList(packingRequest, Packing.class), bookingId));

        List<ReferenceNumbersRequest> referenceNumbersRequests = request.getReferenceNumbersList();
        if (referenceNumbersRequests != null)
            customerBooking.setReferenceNumbersList(referenceNumbersDao.saveEntityFromBooking(commonUtils.convertToEntityList(referenceNumbersRequests, ReferenceNumbers.class), bookingId));

        List<RoutingsRequest> routingsRequest = request.getRoutingList();
        if (routingsRequest != null)
            customerBooking.setRoutingList(routingsDao.saveEntityFromBooking(commonUtils.convertToEntityList(routingsRequest, Routings.class), bookingId));

        List<ContainerV3Request> containerRequest = request.getContainersList();
        if (containerRequest != null) {
            List<Containers> containers = containerDao.updateEntityFromBooking(commonUtils.convertToEntityList(containerRequest, Containers.class), bookingId);
            customerBooking.setContainersList(containers);
        }
    }

    private void processBookingChargesRequest(CustomerBooking customerBooking, List<BookingChargesRequest> bookingChargesRequest, Map<UUID, Containers> containerMap) throws RunnerException {
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
    }

    public void pushCustomerBookingDataToDependentService(CustomerBooking customerBooking , boolean isCreate) {
        try {
            OrderManageDto.OrderManagement orderManagement = OrderManageDto.OrderManagement.builder().orderManagementId(customerBooking.getOrderManagementId()).orderManagementNumber(customerBooking.getOrderManagementNumber()).moduleId(customerBooking.getBookingNumber()).moduleGuid(customerBooking.getGuid().toString()).tenantId(TenantContext.getCurrentTenant()).build();
            KafkaResponse kafkaResponse = producer.getKafkaResponse(orderManagement, isCreate);
            log.info("Producing order management data to kafka with RequestId: {} and payload: {}",LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(kafkaResponse));
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, StringUtility.convertToString(customerBooking.getGuid()));
        }
        catch (Exception e) {
            log.error("Error Producing Order Management Data to kafka, error is due to " + e.getMessage());
        }
    }

    private void contractUtilisationForUpdate(CustomerBooking customerBooking, CustomerBooking old) throws RunnerException {
        if (!Objects.isNull(customerBooking.getContractId()) && Objects.equals(old.getContractId(), customerBooking.getContractId())) {
            // Alteration on same contract
            npmContractUpdate(customerBooking,  old, true, CustomerBookingConstants.REMOVE, false);
        }  else if (!Objects.isNull(customerBooking.getContractId()) && !Objects.isNull(old.getContractId())) {
            // Lock current contract with current containers
            npmContractUpdate(customerBooking, null, false, CustomerBookingConstants.REMOVE, false);
            // Release existing booking with old containers
            npmContractUpdate(old, null, false, CustomerBookingConstants.ADD, false);
        } else if (!Objects.isNull(customerBooking.getContractId()) && Objects.isNull(old.getContractId())) {
            // Lock current contract with current containers
            npmContractUpdate(customerBooking, null, false, CustomerBookingConstants.REMOVE, false);
        }  else if (Objects.isNull(customerBooking.getContractId()) && !Objects.isNull(old.getContractId())) {
            // Release existing booking with old containers
            npmContractUpdate(old, null, false, CustomerBookingConstants.ADD, false);
        }

        if (!Objects.isNull(customerBooking.getContractId()) && Objects.equals(customerBooking.getBookingStatus(), BookingStatus.CANCELLED)) {
            npmContractUpdate(customerBooking, null, false, CustomerBookingConstants.ADD, true);
        }
    }

    private CustomerBooking updateEntities(CustomerBooking customerBooking, CustomerBookingV3Request request, String oldEntity) throws RunnerException {
        populateTotalRevenueDetails(customerBooking, request);
        V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        if(Objects.equals(customerBooking.getBookingStatus(), BookingStatus.READY_FOR_SHIPMENT) && !checkForCreditLimitManagement(customerBooking)){
            throw new RunnerException("Request for credit limit has not been approved. Hence cannot proceed.");
        }

        customerBooking = customerBookingDao.save(customerBooking);
        Long bookingId = customerBooking.getId();

        List<PackingV3Request> packingRequest = request.getPackingList();
        if (packingRequest != null)
            customerBooking.setPackingList(packingDao.updateEntityFromBooking(commonUtils.convertToEntityList(packingRequest, Packing.class), bookingId));

        List<ReferenceNumbersRequest> referenceNumbersRequests = request.getReferenceNumbersList();
        if (referenceNumbersRequests != null)
            customerBooking.setReferenceNumbersList(referenceNumbersDao.updateEntityFromBooking(commonUtils.convertToEntityList(referenceNumbersRequests, ReferenceNumbers.class), bookingId));

        List<RoutingsRequest> routingsRequest = request.getRoutingList();
        if (routingsRequest != null)
            customerBooking.setRoutingList(routingsDao.updateEntityFromBooking(commonUtils.convertToEntityList(routingsRequest, Routings.class), bookingId));

        List<ContainerV3Request> containerRequest = request.getContainersList();
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
            customerBooking = processReadyForShipmentBooking(customerBooking, request, tenantSettingsResponse);
        }
        try {
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(customerBooking)
                            .prevData(jsonHelper.readFromJson(oldEntity, CustomerBooking.class))
                            .parent(CustomerBooking.class.getSimpleName())
                            .parentId(customerBooking.getId())
                            .isIntegrationLog(Objects.equals(customerBooking.getBookingStatus(), BookingStatus.PENDING_FOR_REVIEW))
                            .flow("Inbound")
                            .dataType("Transactional")
                            .operation(DBOperationType.UPDATE.name()).build()
            );
        } catch (Exception e) {
            log.error(e.getMessage());
        }
        return customerBooking;
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
        for(Object[] partyEntry : parties){
            Parties party = (Parties) partyEntry[0];
            if(party == null) continue;
            var orgId = party.getOrgData() != null ? StringUtility.convertToString(party.getOrgData().get(PartiesConstants.ID)) : null;
            var addressId = party.getAddressData() != null ? StringUtility.convertToString(party.getAddressData().get(PartiesConstants.ID)) : null;
            if(StringUtility.isEmpty(orgId) || StringUtility.isEmpty(addressId))
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

        return StringUtils.equals(finalStatus , CustomerBookingConstants.MDM_FINAL_STATUS_APPROVED) || StringUtils.equals(finalStatus , CustomerBookingConstants.MDM_FINAL_STATUS_NO_APPROVAL_NEEDED);
    }

    private CustomerBooking processReadyForShipmentBooking(CustomerBooking customerBooking, CustomerBookingV3Request request, V1TenantSettingsResponse tenantSettingsResponse) throws RunnerException {
        if(Boolean.TRUE.equals(tenantSettingsResponse.getShipmentServiceV2Enabled()))
        {
            Boolean countryAirCargoSecurity = tenantSettingsResponse.getCountryAirCargoSecurity();
            validateAirSecurityAndPermissions(request, countryAirCargoSecurity);
            ShipmentDetailsV3Response shipmentResponse = bookingIntegrationsUtility.createShipmentInV3(request);
            //Check 3
            if(shipmentResponse != null) {
                if(customerBooking.getBookingCharges() != null && !customerBooking.getBookingCharges().isEmpty()) {
                    bookingIntegrationsUtility.createShipment(customerBooking, false, true, jsonHelper.convertValue(shipmentResponse, ShipmentDetailsResponse.class), V1AuthHelper.getHeaders());
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
        }
        else
        {
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
        return customerBooking;
    }

    private void validateAirSecurityAndPermissions(CustomerBookingV3Request request, Boolean countryAirCargoSecurity) {
        if (Boolean.TRUE.equals(countryAirCargoSecurity)) {
            if (!checkAirSecurityForBookingRequest(request))
                throw new ValidationException("User does not have Air Security permission to create AIR EXP Shipment from Booking.");
        } else {
            boolean hasAirDGPermission = UserContext.isAirDgUser();
            if (Objects.equals(request.getTransportType(), Constants.TRANSPORT_MODE_AIR) && Objects.equals(request.getIsDg(), Boolean.TRUE) && !hasAirDGPermission) {
                throw new ValidationException("User does not have AIR DG Permission to create AIR Shipment from Booking");
            }
        }
    }

    public static boolean checkAirSecurityForBookingRequest(CustomerBookingV3Request customerBooking) {
        if (customerBooking.getTransportType().equals(Constants.TRANSPORT_MODE_AIR) && customerBooking.getDirection().equals(DIRECTION_EXP)) {
            return UserContext.isAirSecurityUser();
        }
        return true;
    }

    /**
     * To be used to fetch dependent master-data*
     *
     * @param customerBooking
     * @param customerBookingResponse
     */
    private void createCustomerBookingResponse(CustomerBooking customerBooking, CustomerBookingV3Response customerBookingResponse) {
        try {
            double startTime = System.currentTimeMillis();
            var masterListFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllMasterDataInSingleCall(customerBookingResponse)), executorService);
            var unLocationsFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllLocationDataInSingleCall(customerBookingResponse)), executorService);
            var vesselsFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllVesselDataInSingleCall(customerBookingResponse)), executorService);
            var carrierFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllCarrierDataInSingleCall(customerBookingResponse)), executorService);
            var containerTypeFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllContainerTypesInSingleCall(customerBookingResponse)), executorService);
            var chargeTypeFuture = CompletableFuture.runAsync(withMdc(() -> this.addAllChargeTypesInSingleCall(customerBookingResponse)), executorService);
            if(customerBookingResponse.getBookingStatus() == BookingStatus.READY_FOR_SHIPMENT) {
                V1TenantSettingsResponse tenantSettingsResponse = commonUtils.getCurrentTenantSettings();
                Boolean isShipmentV2 = tenantSettingsResponse.getShipmentServiceV2Enabled();
                if (Boolean.TRUE.equals(isShipmentV2)) {
                    if (customerBookingResponse.getShipmentEntityIdV2() == null) {
                        setShipmentEntityIdV2InResponse(customerBooking, customerBookingResponse);
                    }
                } else {
                    if (customerBookingResponse.getShipmentEntityId() == null) {
                        setShipmentEntityIdInResponse(customerBookingResponse);
                    }
                }
            }
            CompletableFuture.allOf(masterListFuture, unLocationsFuture, vesselsFuture, carrierFuture, containerTypeFuture, chargeTypeFuture).join();
            log.info("Time taken to fetch Master-data from V1: {} ms. || RequestId: {}", System.currentTimeMillis() - startTime, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception ex) {
            log.error("Exception during fetching master data in retrieve API for booking number: {} with exception: {}", customerBooking.getBookingNumber(), ex.getMessage());
        }
    }

    private void setShipmentEntityIdV2InResponse(CustomerBooking customerBooking, CustomerBookingV3Response customerBookingResponse) {
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

    private void setShipmentEntityIdInResponse(CustomerBookingV3Response customerBookingResponse) {
        if (customerBookingResponse.getShipmentGuid() != null) {
            ShipmentBillingListRequest shipmentBillingListRequest = ShipmentBillingListRequest.builder().guidsList(Arrays.asList(UUID.fromString(customerBookingResponse.getShipmentGuid()))).build();
            ShipmentBillingListResponse shipmentBillingListResponse = v1Service.fetchShipmentBillingData(shipmentBillingListRequest);
            if (shipmentBillingListResponse.getData() != null && !shipmentBillingListResponse.getData().isEmpty()) {
                ShipmentBillingListResponse.BillingData billingData = shipmentBillingListResponse.getData().get(customerBookingResponse.getShipmentGuid());
                customerBookingResponse.setShipmentEntityId(StringUtility.convertToString(billingData.getId()));
            }
        }
    }

    public Runnable withMdc(Runnable runnable) {
        Map<String, String> mdc = MDC.getCopyOfContextMap();
        String token = RequestAuthContext.getAuthToken();
        var userContext1 = UserContext.getUser();
        return () -> {
            try {
                MDC.setContextMap(mdc);
                RequestAuthContext.setAuthToken(token);
                UserContext.setUser(userContext1);
                runnable.run();
            } finally {
                RequestAuthContext.removeToken();
                MDC.clear();
                UserContext.removeUser();
            }
        };
    }

    //    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllMasterDataInSingleCall(CustomerBookingV3Response customerBookingResponse) {
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
                customerBookingResponse.getRoutingList().forEach(r -> r.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Routings.class.getSimpleName() + r.getId() ), CacheConstants.MASTER_LIST, true, cacheMap)));
            if (!Objects.isNull(customerBookingResponse.getContainersList()))
                customerBookingResponse.getContainersList().forEach(c -> c.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + c.getId() ), CacheConstants.MASTER_LIST, true, cacheMap)));
            if (!Objects.isNull(customerBookingResponse.getPackingList()))
                customerBookingResponse.getPackingList().forEach(c -> c.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Packing.class.getSimpleName() + c.getId() ), CacheConstants.MASTER_LIST, true, cacheMap)));
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CustomerBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }

    }

    //    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllLocationDataInSingleCall(CustomerBookingV3Response customerBookingResponse) {
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
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllChargeTypesInSingleCall(CustomerBookingV3Response customerBookingResponse) {
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
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllContainerTypesInSingleCall(CustomerBookingV3Response customerBookingResponse) {
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
        }  catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllContainerTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CustomerBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    //    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllVesselDataInSingleCall(CustomerBookingV3Response customerBookingResponse) {
        try {
            if (!Objects.isNull(customerBookingResponse.getCarrierDetails())) {
                Map<String, Object> cacheMap = new HashMap<>();
                Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
                Set<String> vesselList = new HashSet<>(masterDataUtils.createInBulkVesselsRequest(customerBookingResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap));
                Map<String, EntityTransferVessels> v1Data = masterDataUtils.fetchInBulkVessels(vesselList);
                masterDataUtils.pushToCache(v1Data, CacheConstants.VESSELS, vesselList, new EntityTransferVessels(), cacheMap);
                customerBookingResponse.getCarrierDetails().setVesselsMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.VESSELS, true, cacheMap));
            }
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(Arrays.asList()));
        }  catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllVesselDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CustomerBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }

    //    @Async
    private CompletableFuture<ResponseEntity<IRunnerResponse>> addAllCarrierDataInSingleCall(CustomerBookingV3Response customerBookingResponse) {
        try {
            if (!Objects.isNull(customerBookingResponse.getCarrierDetails())) {
                Map<String, Object> cacheMap = new HashMap<>();
                Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
                Set<String> carriersList = new HashSet<>(masterDataUtils.createInBulkCarriersRequest(customerBookingResponse.getCarrierDetails(), CarrierDetails.class, fieldNameKeyMap, CarrierDetails.class.getSimpleName(), cacheMap));
                Map<String, EntityTransferCarrier> v1Data = masterDataUtils.fetchInBulkCarriers(carriersList);
                masterDataUtils.pushToCache(v1Data, CacheConstants.CARRIER, carriersList, new EntityTransferCarrier(), cacheMap);
                customerBookingResponse.getCarrierDetails().setCarrierMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(CarrierDetails.class.getSimpleName()), CacheConstants.CARRIER, true, cacheMap));
            }
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(Arrays.asList()));
        }  catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCarrierDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), CustomerBookingService.class.getSimpleName(), ex.getMessage());
            return CompletableFuture.completedFuture(null);
        }
    }
}
