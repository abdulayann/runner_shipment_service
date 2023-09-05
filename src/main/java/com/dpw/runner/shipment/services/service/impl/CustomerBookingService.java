package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.ICRPServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.IFusionServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.INPMServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.crp.CRPRetrieveRequest;
import com.dpw.runner.shipment.services.dto.request.npm.*;
import com.dpw.runner.shipment.services.dto.request.npm.HazardousInfoRequest;
import com.dpw.runner.shipment.services.dto.request.platformBooking.PlatformToRunnerCustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.platform.*;
import com.dpw.runner.shipment.services.dto.request.platform.AirCarrierDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.CRPRetrieveResponse;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.dto.response.PlatformToRunnerCustomerBookingResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1ShipmentCreationResponse;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingSource;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.exception.exceptions.CRPException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.*;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.convertToEntityList;

@Service
@Slf4j
public class CustomerBookingService implements ICustomerBookingService {
    @Autowired
    private ModelMapper modelMapper;

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
    private IFileRepoDao fileRepoDao;


    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private IFusionServiceAdapter fusionServiceAdapter;

    @Autowired
    private INPMServiceAdapter npmService;

    @Autowired
    UserContext userContext;

    @Autowired
    private BookingIntegrationsUtility bookingIntegrationsUtility;

    @Autowired
    private ICRPServiceAdapter crpServiceAdapter;
    @Autowired
    private AuditLogService auditLogService;
    @Autowired
    private IV1Service v1Service;

    private static final Map<String, String> loadTypeMap = Map.of("SEA", "LCL", "AIR", "LSE");

    private Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("customerOrgCode", RunnerEntityMapping.builder().tableName("customer").dataType(String.class).fieldName("orgCode").build()),
            Map.entry("consignerOrgCode", RunnerEntityMapping.builder().tableName("consignor").dataType(String.class).fieldName("orgCode").build()),
            Map.entry("consigneeOrgCode", RunnerEntityMapping.builder().tableName("consignee").dataType(String.class).fieldName("orgCode").build()),
            Map.entry("origin", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("origin").build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("destination").build()),
            Map.entry("originPort", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("originPort").build()),
            Map.entry("destinationPort", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).fieldName("destinationPort").build()),
            Map.entry("bookingNumber", RunnerEntityMapping.builder().tableName("CustomerBooking").dataType(String.class).fieldName("bookingNumber").isContainsText(true).build()),
            Map.entry("bookingDate", RunnerEntityMapping.builder().tableName("CustomerBooking").dataType(LocalDateTime.class).fieldName("bookingDate").build()),
            Map.entry("bookingStatus", RunnerEntityMapping.builder().tableName("CustomerBooking").dataType(BookingStatus.class).fieldName("bookingStatus").build()),
            Map.entry("createdBy", RunnerEntityMapping.builder().tableName("CustomerBooking").dataType(String.class).fieldName("createdBy").build())
    );


    @Override
    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) throws Exception {

        CustomerBookingRequest request = (CustomerBookingRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Customer Booking Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        CustomerBooking customerBooking = jsonHelper.convertValue(request, CustomerBooking.class);
        customerBooking.setSource(BookingSource.Runner);

        npmContractUpdate(customerBooking, false, null);  // NPM update contract
        try {
            createEntities(customerBooking, request);
            /**
             * Platform service integration
             * Criteria for update call to platform service : check flag IsPlatformBookingCreated, if true then update otherwise dont update
             */
            if (!Objects.isNull(customerBooking.getBusinessCode()) && Objects.equals(customerBooking.getBookingStatus(), BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                    && !Objects.isNull(customerBooking.getBookingCharges()) && !customerBooking.getBookingCharges().isEmpty()) {
                bookingIntegrationsUtility.createBookingInPlatform(customerBooking);
            }
        } catch (Exception e) {
            log.error(e.getMessage());
            throw e;
        }
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(customerBooking, CustomerBookingResponse.class));
    }

    private void createEntities(CustomerBooking customerBooking, CustomerBookingRequest request) throws Exception {
        if (customerBooking.getIsPlatformBookingCreated() == null) {
            customerBooking.setIsPlatformBookingCreated(false);
        }
        if (customerBooking.getBookingNumber() == null) {
            customerBooking.setBookingNumber(generateBookingNumber(customerBooking.getCargoType()));
        }
        customerBooking = customerBookingDao.save(customerBooking);
        Long bookingId = customerBooking.getId();

        List<PackingRequest> packingRequest = request.getPackingList();
        if (packingRequest != null)
            customerBooking.setPackingList(packingDao.saveEntityFromBooking(convertToEntityList(packingRequest, Packing.class), bookingId));

        List<FileRepoRequest> fileRepoRequest = request.getFileRepoList();
        if (fileRepoRequest != null)
            customerBooking.setFileRepoList(fileRepoDao.saveEntityFromOtherEntity(convertToEntityList(fileRepoRequest, FileRepo.class), bookingId, Constants.BOOKING));

        List<RoutingsRequest> routingsRequest = request.getRoutingList();
        if (routingsRequest != null)
            customerBooking.setRoutingList(routingsDao.saveEntityFromBooking(convertToEntityList(routingsRequest, Routings.class), bookingId));

        List<ContainerRequest> containerRequest = request.getContainersList();
        if (containerRequest != null) {
            List<Containers> containers = containerDao.updateEntityFromBooking(convertToEntityList(containerRequest, Containers.class), bookingId);
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
                bookingCharge = bookingChargesDao.save(bookingCharge);
                bookingCharges.add(bookingCharge);
            }
            customerBooking.setBookingCharges(bookingCharges);
        }
        auditLogService.addAuditLog(
                AuditLogMetaData.builder()
                        .newData(customerBooking)
                        .prevData(null)
                        .parent(CustomerBooking.class.getSimpleName())
                        .parentId(customerBooking.getId())
                        .operation(DBOperationType.CREATE.name()).build()
        );
    }

    @Override
    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
        CustomerBookingRequest request = (CustomerBookingRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Booking update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if (request.getId() == null) {
            log.error("Request Id is null for Booking update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<CustomerBooking> oldEntity = customerBookingDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Booking Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
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

        // NPM update contract
        if (oldEntity.get().getBookingCharges() != null) {
            npmContractUpdate(customerBooking, true, oldEntity.get());
        } else {
            npmContractUpdate(customerBooking, false, oldEntity.get());
        }
        customerBooking = this.updateEntities(customerBooking, request, jsonHelper.convertToJson(oldEntity.get()));
        if (!Objects.isNull(customerBooking.getBusinessCode()) && Objects.equals(customerBooking.getBookingStatus(), BookingStatus.PENDING_FOR_CREDIT_LIMIT)
                && !customerBooking.getBookingCharges().isEmpty() && !isCreatedInPlatform) {
            bookingIntegrationsUtility.createBookingInPlatform(customerBooking);
        } else if (isCreatedInPlatform) {
            bookingIntegrationsUtility.updateBookingInPlatform(customerBooking);
        }

        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(customerBooking, CustomerBookingResponse.class));
    }

    private CustomerBooking updateEntities(CustomerBooking customerBooking, CustomerBookingRequest request, String oldEntity) throws Exception {
        customerBooking = customerBookingDao.save(customerBooking);
        Long bookingId = customerBooking.getId();

        List<PackingRequest> packingRequest = request.getPackingList();
        if (packingRequest != null)
            customerBooking.setPackingList(packingDao.updateEntityFromBooking(convertToEntityList(packingRequest, Packing.class), bookingId));

        List<FileRepoRequest> fileRepoRequest = request.getFileRepoList();
        if (fileRepoRequest != null)
            customerBooking.setFileRepoList(fileRepoDao.updateEntityFromOtherEntity(convertToEntityList(fileRepoRequest, FileRepo.class), bookingId, Constants.BOOKING));

        List<RoutingsRequest> routingsRequest = request.getRoutingList();
        if (routingsRequest != null)
            customerBooking.setRoutingList(routingsDao.updateEntityFromBooking(convertToEntityList(routingsRequest, Routings.class), bookingId));

        List<ContainerRequest> containerRequest = request.getContainersList();
        if (containerRequest != null) {
            List<Containers> containers = containerDao.updateEntityFromBooking(convertToEntityList(containerRequest, Containers.class), bookingId);
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
            bookingCharges = bookingChargesDao.updateEntityFromBooking(bookingCharges, bookingId);
            customerBooking.setBookingCharges(bookingCharges);
        }
        if (Objects.equals(customerBooking.getBookingStatus(), BookingStatus.READY_FOR_SHIPMENT)) {
            V1ShipmentCreationResponse shipmentCreationResponse = jsonHelper.convertValue(bookingIntegrationsUtility.createShipmentInV1(customerBooking).getBody(), V1ShipmentCreationResponse.class);
            if (!Objects.isNull(shipmentCreationResponse) && !Objects.isNull(shipmentCreationResponse.getShipmentId())) {
                customerBooking.setShipmentId(shipmentCreationResponse.getShipmentId());
                customerBooking = customerBookingDao.save(customerBooking);
            }
        }
        auditLogService.addAuditLog(
                AuditLogMetaData.builder()
                        .newData(customerBooking)
                        .prevData(jsonHelper.readFromJson(oldEntity, CustomerBooking.class))
                        .parent(CustomerBooking.class.getSimpleName())
                        .parentId(customerBooking.getId())
                        .operation(DBOperationType.UPDATE.name()).build()
        );
        return customerBooking;
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Booking list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
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
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Booking async list for Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<CustomerBooking>, Pageable> tuple = fetchData(request, CustomerBooking.class, tableNames);
            Page<CustomerBooking> customerBookingPage = customerBookingDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Booking async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(customerBookingPage.getContent()),
                    customerBookingPage.getTotalPages(),
                    customerBookingPage.getTotalElements()));
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
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.debug("Request is empty for Booking delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for Booking delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<CustomerBooking> customerBooking = customerBookingDao.findById(id);
            if (!customerBooking.isPresent()) {
                log.debug("Booking Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
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
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Booking retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Booking retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<CustomerBooking> customerBooking = customerBookingDao.findById(id);
            if (!customerBooking.isPresent()) {
                log.debug("Booking Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Booking details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            CustomerBookingResponse customerBookingResponse = jsonHelper.convertValue(customerBooking.get(), CustomerBookingResponse.class);
            populateTotalRevenueDetails(customerBooking.get(), customerBookingResponse);
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
    public ResponseEntity<?> checkCreditLimitFromFusion(CommonRequestModel commonRequestModel) throws Exception {
        CreditLimitRequest creditLimitRequest = (CreditLimitRequest) commonRequestModel.getData();
        V1RetrieveResponse v1RetrieveResponse = v1Service.retrieveTenantSettings();

        V1TenantSettingsResponse v1TenantSettingsResponse = modelMapper.map(v1RetrieveResponse.getEntity(), V1TenantSettingsResponse.class);
        int tenantId = userContext.getUser().TenantId;
        List<Object> criteria = new ArrayList<>();
        List<Object> field = new ArrayList<>(List.of(CustomerBookingConstants.TENANT_ID));
        String operator = "=";
        criteria.addAll(List.of(field, operator, tenantId));
        V1DataResponse tenantName = v1Service.tenantNameByTenantId(CommonV1ListRequest.builder().criteriaRequests(criteria).build());

        V1TenantResponse v1TenantResponse = modelMapper.map(((ArrayList) tenantName.entities).get(0), V1TenantResponse.class);

        if (!v1TenantSettingsResponse.getEnableCreditLimitManagement() || !v1TenantSettingsResponse.getIsCreditLimitWithFusionEnabled()) {
            log.error("EnableCreditLimitManagement Or EnableCreditLimitIntegrationWithFusion is False in Branch settings with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("EnableCreditLimitManagement Or EnableCreditLimitIntegrationWithFusion is False in Branch settings");
        }
        boolean isCustomerBookingRestricted = v1TenantSettingsResponse.getRestrictedItemsForCreditLimit().stream().anyMatch(p -> p.equals("CUS_BK"));
        if (!isCustomerBookingRestricted) {
            log.error("'Restrict the transaction when Credit Limit is enabled' does not include CustomerBooking with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("'Restrict the transaction when Credit Limit is enabled' does not include CustomerBooking");
        }
        CheckCreditBalanceFusionRequest request = CheckCreditBalanceFusionRequest.builder().req_Params(new CheckCreditBalanceFusionRequest.ReqParams()).build();
        if(v1TenantSettingsResponse.getCreditLimitOn() == 0){
            if(creditLimitRequest == null || creditLimitRequest.getCustomerIdentifierId() == null){
                log.error("CustomerIdentifierId is Required with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException("CustomerIdentifierId is Required for credit check");
            }
            request.getReq_Params().setAccount_number(creditLimitRequest.getCustomerIdentifierId());
        }
        else if(v1TenantSettingsResponse.getCreditLimitOn() == 1) {
            if(creditLimitRequest == null || creditLimitRequest.getSiteIdentifierId() == null){
                log.error("SiteIdentifierId is Required with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException("SiteIdentifierId is Required for credit check");
            }
            request.getReq_Params().setSite_number(creditLimitRequest.getSiteIdentifierId());
        }
        if (v1TenantSettingsResponse.getIsGlobalFusionIntegrationEnabled()) {
            request.getReq_Params().setCalling_System(CustomerBookingConstants.GCR_FUSION);
            request.getReq_Params().setBu_id(v1TenantSettingsResponse.getBusinessUnitName());
            ResponseEntity<DependentServiceResponse> response = (ResponseEntity<DependentServiceResponse>) fusionServiceAdapter.checkCreditLimitP100(CommonRequestModel.buildRequest(request));
            if(response.getBody() == null || response.getBody().getData() == null){
                log.error("No Data found on Fusion with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new ValidationException("No Data found on Fusion");
            }
            CheckCreditBalanceFusionResponse checkCreditBalanceFusionResponse = modelMapper.map(response.getBody().getData(), CheckCreditBalanceFusionResponse.class);
            CheckCreditLimitResponse checkCreditLimitResponse = createCheckCreditLimitPayload(checkCreditBalanceFusionResponse);
            return ResponseHelper.buildSuccessResponse(checkCreditLimitResponse);
        } else {
            log.error("'Enable Global Fusion Integration' is false for this Tenant this is required for Customer Booking with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("'Enable Global Fusion Integration' is false for this Tenant this is required for Customer Booking");
        }
    }

    private CheckCreditLimitResponse createCheckCreditLimitPayload(CheckCreditBalanceFusionResponse checkCreditBalanceFusionResponse){
        if(checkCreditBalanceFusionResponse.getData().getCreditDetails() == null || checkCreditBalanceFusionResponse.getData().getCreditDetails().isEmpty()){
            log.error("No Data found on Fusion with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("No Data found on Fusion: "+ checkCreditBalanceFusionResponse.getData().getMessage());
        }
        double totalCreditLimit = checkCreditBalanceFusionResponse.getData().getCreditDetails().get(0).getTotalCreditLimit();
        double outstandingAmount = checkCreditBalanceFusionResponse.getData().getCreditDetails().get(0).getOutstandingAmount();
        double overDueAmount = checkCreditBalanceFusionResponse.getData().getCreditDetails().get(0).getOverDue();
        double totalCreditAvailableBalance = (totalCreditLimit - outstandingAmount);

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
                .build();
    }

    @Override
    @Transactional
    public ResponseEntity<?> platformCreateBooking(CommonRequestModel commonRequestModel) throws Exception {
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
                pack.setLengthUnit(Constants.METRE);
                pack.setWidthUnit(Constants.METRE);
                pack.setHeightUnit(Constants.METRE);
                pack.setIsDimension(true);
                try {
                    if (pack.getDimensionUnit() != null && !pack.getDimensionUnit().equals(Constants.METRE)) {
                        pack.setLength(BigDecimal.valueOf(
                                UnitConversionUtility.convertUnit(Constants.LENGTH, pack.getLength(), pack.getDimensionUnit(), Constants.METRE)
                                        .doubleValue()));
                        pack.setWidth(BigDecimal.valueOf(
                                UnitConversionUtility.convertUnit(Constants.LENGTH, pack.getWidth(), pack.getDimensionUnit(), Constants.METRE)
                                        .doubleValue()));
                        pack.setHeight(BigDecimal.valueOf(
                                UnitConversionUtility.convertUnit(Constants.LENGTH, pack.getHeight(), pack.getDimensionUnit(), Constants.METRE)
                                        .doubleValue()));
                        pack.setDimensionUnit(Constants.METRE);
                    }
                    if (pack.getWeightUnit() != null && !pack.getWeightUnit().equals(Constants.WEIGHT_UNIT_KG)) {
                        pack.setWeight(BigDecimal.valueOf(
                                UnitConversionUtility.convertUnit(Constants.MASS, pack.getWeight(), pack.getWeightUnit(), Constants.WEIGHT_UNIT_KG)
                                        .doubleValue()));
                        pack.setWeightUnit(Constants.WEIGHT_UNIT_KG);
                    }
                    pack.setVolume(pack.getLength().multiply(pack.getWidth()).multiply(pack.getHeight()));
                    pack.setVolumeUnit(Constants.VOLUME_UNIT_M3);
                } catch (Exception ex) {
                    String message = "ERROR Exception thrown while converting units";
                    log.error("ERROR Converting units");
                }
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

                if (charge.getContainers() != null) {
                    charge.getContainers().forEach(cont -> {
                        if (cont.getRunner_guid() != null) {
                            if (charge.getContainersUUID() != null)
                                charge.getContainersUUID().add(cont.getRunner_guid());
                            else
                                charge.setContainersUUID(List.of(cont.getRunner_guid()));
                        } else {
                            if (charge.getContainersUUID() != null) {
                                if (referenceIdVsGuidContainerMap.containsKey(cont.getReference_id())) {
                                    charge.getContainersUUID().add(referenceIdVsGuidContainerMap.get(cont.getReference_id()));
                                }
                            } else {
                                if (referenceIdVsGuidContainerMap.containsKey(cont.getReference_id())) {
                                    charge.setContainersUUID(List.of(referenceIdVsGuidContainerMap.get(cont.getReference_id())));
                                }
                            }
                        }
                    });
                }
            });
            platformResponse.setCharges(referenceNumbersGuidMapResponses);
        }

        CustomerBookingRequest customerBookingRequest = modelMapper.map(request, CustomerBookingRequest.class);
        assignCarrierDetailsToRequest(customerBookingRequest, request);
        ResponseEntity<RunnerResponse<CustomerBookingResponse>> response = null;
        if (customerBooking.isEmpty()) {
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
        if (request.getCustomer() != null) {
            String orgCode = request.getCustomer().getOrgCode();
            String addressCode = request.getCustomer().getAddressCode();
            transformOrgAndAddressPayload(request.getCustomer(), addressCode, orgCode);
        }
        if ((Objects.isNull(request.getIsConsignorFreeText()) || request.getIsConsignorFreeText()) && request.getConsignor() != null) {
            transformOrgAndAddressToRawData(request.getConsignor());
        }
        if ((Objects.isNull(request.getIsConsigneeFreeText()) || request.getIsConsigneeFreeText()) && request.getConsignee() != null) {
            transformOrgAndAddressToRawData(request.getConsignee());
        }
        if ((Objects.isNull(request.getIsNotifyPartyFreeText()) || request.getIsNotifyPartyFreeText()) && request.getNotifyParty() != null) {
            transformOrgAndAddressToRawData(request.getNotifyParty());
        }

        if (request.getBookingCharges() != null && !request.getBookingCharges().isEmpty()) {
            request.getBookingCharges().forEach(charge -> {
                if (charge.getCreditor() != null) {
                    String orgCode = charge.getCreditor().getOrgCode();
                    String addressCode = charge.getCreditor().getAddressCode();
                    transformOrgAndAddressPayload(charge.getCreditor(), addressCode, orgCode);
                }
                if (charge.getDebtor() != null) {
                    String orgCode = charge.getDebtor().getOrgCode();
                    String addressCode = charge.getDebtor().getAddressCode();
                    transformOrgAndAddressPayload(charge.getDebtor(), addressCode, orgCode);
                } else {
                    if (charge.getCreditor() != null)
                        charge.setDebtor(charge.getCreditor());
                }
            });

        }
    }

    private void transformOrgAndAddressToRawData(PartiesRequest partiesRequest) {
        Map<String, Object> orgData = partiesRequest.getOrgData();
        Map<String, Object> addressData = partiesRequest.getAddressData();

        String orgString = "";
        String addressString = "";
        if (orgData.containsKey(PartiesConstants.FULLNAME)) {
            orgString = orgString.concat((String) orgData.get(PartiesConstants.FULLNAME));
            addressString = addressString.concat((String) orgData.get(PartiesConstants.FULLNAME) + "|");
        }
        partiesRequest.setOrgData(Map.of(PartiesConstants.RAW_DATA, orgString));
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
        partiesRequest.setAddressData(Map.of(PartiesConstants.RAW_DATA, addressString));
    }

    private void transformOrgAndAddressPayload(PartiesRequest request, String addressCode, String orgCode) {

        CRPRetrieveRequest retrieveRequest = CRPRetrieveRequest.builder().searchString(orgCode).build();
        var response = new CRPRetrieveResponse();
        try {
            ResponseEntity<DependentServiceResponse> crpResponse = (ResponseEntity<DependentServiceResponse>) crpServiceAdapter.retrieveCRPService(CommonRequestModel.buildRequest(retrieveRequest));
            response = modelMapper.map(crpResponse.getBody().getData(), CRPRetrieveResponse.class);
        } catch (Exception e) {
            log.error("CRP Retrieve failed due to: " + e.getMessage());
            throw new CRPException(e.getMessage());
        }
        if (response == null) {
            log.error("No organization exist in CRP with OrgCode: " + orgCode);
            throw new DataRetrievalFailureException("No organization exist in CRP with OrgCode: " + orgCode);
        }

        Map<String, Object> orgData = new HashMap<>();
        Map<String, Object> addressData = new HashMap<>();
        CRPRetrieveResponse.CRPAddressDetails crpAddressDetails = new CRPRetrieveResponse.CRPAddressDetails();
        if (response.getCompanyOfficeDetails() != null) {
            List<CRPRetrieveResponse.CRPAddressDetails> crpAddressDetailsList = response.getCompanyOfficeDetails().stream().filter(x -> Objects.equals(x.getOfficeReference(), addressCode)).collect(Collectors.toList());
            if (!crpAddressDetailsList.isEmpty())
                crpAddressDetails = crpAddressDetailsList.get(0);
        }
        String customerIdentifier = null;
        if (response.getCompanyAttributesDetails() != null) {
            List<CRPRetrieveResponse.CompanyAttributesDetail> companyAttributesDetailList = response.getCompanyAttributesDetails().stream().filter(x -> x.getAttributeNameAttributeValuePair() != null && Objects.equals(x.getAttributeNameAttributeValuePair().getKey(), PartiesConstants.ACCOUNT_ID)).toList();
            if (!companyAttributesDetailList.isEmpty()) {
                customerIdentifier = companyAttributesDetailList.get(0).getAttributeNameAttributeValuePair().getValue();
            }
        }

        String fusionSiteIdentifier = null;
        String billableFlag = "";
        if (response.getCompanyCodeIssuerDetails() != null) {
            List<CRPRetrieveResponse.CompanyCodeIssuerDetails> companyCodeIssuerDetailsList = response.getCompanyCodeIssuerDetails().stream().filter(x -> Objects.equals(x.getIdentifierValue(), addressCode)).collect(Collectors.toList());
            if (!companyCodeIssuerDetailsList.isEmpty()) {
                var fusionSiteIdList = companyCodeIssuerDetailsList.stream().filter(x -> Objects.equals(x.getIdentifierCodeType(), PartiesConstants.FUSION_BILL_TO_SITE_NUMBER) && Objects.equals(x.getIdentifierIssuedBy(), PartiesConstants.FUSION)).collect(Collectors.toList());
                var billableFlagList = companyCodeIssuerDetailsList.stream().filter(x -> Objects.equals(x.getIdentifierCodeType(), PartiesConstants.BILLABLE_FLAG)).collect(Collectors.toList());
                if (!fusionSiteIdList.isEmpty())
                    fusionSiteIdentifier = fusionSiteIdList.get(0).getIdentifierCode();
                if (!billableFlagList.isEmpty())
                    billableFlag = billableFlagList.get(0).getIdentifierCode();
            }
        }

        orgData.put(PartiesConstants.ORGANIZATION_CODE, response.getCompanyReference());
        orgData.put(PartiesConstants.FULLNAME, response.getCompanyName());
        orgData.put(PartiesConstants.ADDRESS1, response.getAddressLine1());
        orgData.put(PartiesConstants.ADDRESS2, response.getAddressLine2());
        orgData.put(PartiesConstants.COUNTRY, response.getCountryCode());
        orgData.put(PartiesConstants.CITY_CODE, response.getCityName());
        orgData.put(PartiesConstants.STATE, response.getStateName());
        orgData.put(PartiesConstants.ZIP_POST_CODE, response.getPostalCode());
        orgData.put(PartiesConstants.MOBILE, response.getContactNumber());
        orgData.put(PartiesConstants.EMAIL, response.getCompanyEmail());
        orgData.put(PartiesConstants.ACTIVE_CLIENT, true);
        orgData.put(PartiesConstants.DEFAULT_ADDRESS_SITE_IDENTIFIER, fusionSiteIdentifier);
        orgData.put(PartiesConstants.RECEIVABLES, true);     // This is hardcoded to true in case of CRP as asked by product
        orgData.put(PartiesConstants.CUSTOMER_IDENTIFIER, customerIdentifier);

        request.setOrgData(orgData);

        addressData.put(PartiesConstants.ADDRESS_SHORT_CODE, crpAddressDetails.getOfficeReference());
        addressData.put(PartiesConstants.COMPANY_NAME, crpAddressDetails.getOfficeName());
        addressData.put(PartiesConstants.SITE_IDENTIFIER, fusionSiteIdentifier);
        addressData.put(PartiesConstants.ADDRESS1, crpAddressDetails.getAddress());
        addressData.put(PartiesConstants.COUNTRY, response.getCountryCode());
        addressData.put(PartiesConstants.CITY, crpAddressDetails.getCityName());
        addressData.put(PartiesConstants.STATE, crpAddressDetails.getStateName());
        addressData.put(PartiesConstants.ZIP_POST_CODE, crpAddressDetails.getPostalCode());
        addressData.put(PartiesConstants.MOBILE, response.getContactNumber());
        addressData.put(PartiesConstants.EMAIL, crpAddressDetails.getOfficeEmail());

        request.setAddressData(addressData);
    }

    private CustomerBookingResponse updatePlatformBooking(CustomerBookingRequest request, CustomerBooking oldEntity) throws Exception {
        CustomerBooking customerBooking = jsonHelper.convertValue(request, CustomerBooking.class);
        customerBooking.setIsPlatformBookingCreated(Boolean.TRUE);
        customerBooking.setSource(BookingSource.Platform);
        try {
            customerBooking = this.updateEntities(customerBooking, request, jsonHelper.convertToJson(oldEntity));
        } catch (Exception e) {
            log.error(e.getMessage());
            throw e;
        }
        return jsonHelper.convertValue(customerBooking, CustomerBookingResponse.class);
    }

    private CustomerBookingResponse createPlatformBooking(CustomerBookingRequest request) throws Exception {
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
            throw e;
        }
        return jsonHelper.convertValue(customerBooking, CustomerBookingResponse.class);
    }

    private void assignCarrierDetailsToRequest(CustomerBookingRequest customerBookingRequest, PlatformToRunnerCustomerBookingRequest request) {

        CarrierDetailRequest carrierDetailRequest = CarrierDetailRequest.builder()
                .origin(request.getOrigin())
                .destination(request.getDestination())
                .originPort(request.getOriginPort())
                .destinationPort(request.getDestinationPort())
                .shippingLine(request.getShippingLine())
                .vessel(request.getVessel())
                .voyage(request.getVoyage())
                .build();

        customerBookingRequest.setCarrierDetails(carrierDetailRequest);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<CustomerBooking> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(customerBooking -> {
            CustomerBookingResponse response = modelMapper.map(customerBooking, CustomerBookingResponse.class);
            responseList.add(response);
        });
        masterDataUtils.setLocationData(responseList);
        return responseList;
    }

    private void npmContractUpdate(CustomerBooking customerBooking, Boolean isAlteration, CustomerBooking oldEntity) throws Exception {
        if (customerBooking.getTransportType() != null && customerBooking.getTransportType().equals(Constants.TRANSPORT_MODE_SEA) && customerBooking.getCargoType() != null && customerBooking.getCargoType().equals(Constants.CARGO_TYPE_FCL) &&
                StringUtility.isNotEmpty(customerBooking.getContractId()) && customerBooking.getBookingCharges() != null && !customerBooking.getBookingCharges().isEmpty()) {

            List<LoadInfoRequest> loadInfoRequestList = containersListForLoad(customerBooking, oldEntity, customerBooking.getBookingStatus() == BookingStatus.CANCELLED);
            if (!loadInfoRequestList.isEmpty())
                npmContractUpdatePaylaod(customerBooking, isAlteration, loadInfoRequestList);
        }
    }

    private List<LoadInfoRequest> containersListForLoad(CustomerBooking customerBooking, CustomerBooking oldEntity, Boolean isCancelled) {
        Map<Long, Containers> idVsContainerMap;
        if (oldEntity != null && oldEntity.getContainersList() != null) {
            idVsContainerMap = oldEntity.getContainersList().stream().collect(Collectors.toMap(Containers::getId, c -> c));
        } else {
            idVsContainerMap = new HashMap<>();
        }
        List<LoadInfoRequest> loadInfoRequestList = new ArrayList<>();
        if (!isCancelled && customerBooking.getContainersList() != null) {
            customerBooking.getContainersList().forEach(cont -> {
                if (cont.getId() == null) {
                    loadInfoRequestList.add(containerLoadConstruct(cont, CustomerBookingConstants.REMOVE, cont.getContainerCount()));
                } else {
                    if (idVsContainerMap.containsKey(cont.getId())) {
                        if (!cont.getContainerCode().equals(idVsContainerMap.get(cont.getId()).getContainerCode())) {
                            loadInfoRequestList.add(containerLoadConstruct(cont, CustomerBookingConstants.REMOVE, cont.getContainerCount()));
                            loadInfoRequestList.add(containerLoadConstruct(idVsContainerMap.get(cont.getId()), CustomerBookingConstants.ADD, idVsContainerMap.get(cont.getId()).getContainerCount()));

                        } else if (!cont.getCommodityGroup().equals(idVsContainerMap.get(cont.getId()).getCommodityGroup())) {
                            loadInfoRequestList.add(containerLoadConstruct(cont, CustomerBookingConstants.REMOVE, cont.getContainerCount()));
                            loadInfoRequestList.add(containerLoadConstruct(idVsContainerMap.get(cont.getId()), CustomerBookingConstants.ADD, idVsContainerMap.get(cont.getId()).getContainerCount()));
                        } else if (cont.getContainerCount() > idVsContainerMap.get(cont.getId()).getContainerCount()) {
                            Long containerCount = cont.getContainerCount() - idVsContainerMap.get(cont.getId()).getContainerCount();
                            loadInfoRequestList.add(containerLoadConstruct(cont, CustomerBookingConstants.REMOVE, containerCount));
                        } else if (cont.getContainerCount() < idVsContainerMap.get(cont.getId()).getContainerCount()) {
                            Long containerCount = idVsContainerMap.get(cont.getId()).getContainerCount() - cont.getContainerCount();
                            loadInfoRequestList.add(containerLoadConstruct(cont, CustomerBookingConstants.ADD, containerCount));
                        }
                        idVsContainerMap.remove(cont.getId());
                    }
                }
            });
            if (!idVsContainerMap.isEmpty() && StringUtility.isNotEmpty(oldEntity.getContractId()) && oldEntity.getContractId().equals(customerBooking.getContractId())) {
                idVsContainerMap.values().forEach(cont -> {
                    loadInfoRequestList.add(containerLoadConstruct(cont, CustomerBookingConstants.ADD, cont.getContainerCount()));
                });
            }
        } else if (isCancelled && oldEntity != null && oldEntity.getContainersList() != null && StringUtility.isNotEmpty(oldEntity.getContractId()) && oldEntity.getContractId().equals(customerBooking.getContractId())) {
            oldEntity.getContainersList().forEach(cont -> {
                loadInfoRequestList.add(containerLoadConstruct(cont, CustomerBookingConstants.ADD, cont.getContainerCount()));
            });
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

    private void npmContractUpdatePaylaod(CustomerBooking customerBooking, Boolean isAlteration, List<LoadInfoRequest> loadInfoRequestList) throws Exception {
        String contractStatus = null;
        if (customerBooking.getContractStatus() != null && customerBooking.getContractStatus().equals(CustomerBookingConstants.SINGLE_USAGE) && customerBooking.getBookingStatus() == BookingStatus.CANCELLED) {
            contractStatus = CustomerBookingConstants.ENABLED;
        } else if (customerBooking.getContractStatus() != null && customerBooking.getContractStatus().equals(CustomerBookingConstants.SINGLE_USAGE)) {
            contractStatus = CustomerBookingConstants.DISABLED;
        }

        UpdateContractRequest updateContractRequest = UpdateContractRequest.builder()
                .contract_id(customerBooking.getContractId())
                .contract_state(contractStatus)
                .source(CustomerBookingConstants.RUNNER)
                .source_type(CustomerBookingConstants.RUNNER)
                .business_info(UpdateContractRequest.BusinessInfo.builder().product_name("FCL").build())
                .loads_info(loadInfoRequestList)
                .is_alteration(isAlteration)
                .build();

        npmService.updateContracts(CommonRequestModel.buildRequest(updateContractRequest));
    }

    private String generateBookingNumber(String cargoType) {
        String prefix = "DBAR";
        if (Objects.equals(cargoType, "FCL"))
            prefix = "DBFC";
        else if (Objects.equals(cargoType, "LCL"))
            prefix = "DBLC";
        return prefix + "-" + getRandomNumberString(7) + "-" + getRandomNumberString(6);
    }

    public static String getRandomNumberString(int digit) {
        Random rnd = new Random();
        int number = 0;
        if (digit == 7)
            number = rnd.nextInt(9999999);
        else
            number = rnd.nextInt(999999);
        if (digit == 7)
            return String.format("%07d", number);
        return String.format("%06d", number);
    }

    /**
     * To be used to fetch dependent masterdata*
     *
     * @param customerBooking
     * @param customerBookingResponse
     */
    private void createCustomerBookingResponse(CustomerBooking customerBooking, CustomerBookingResponse customerBookingResponse) {
        this.addAllMasterDatas(customerBooking, customerBookingResponse);
        this.addAllUnlocationDatas(customerBooking, customerBookingResponse);
        this.addDedicatedMasterData(customerBooking, customerBookingResponse);
    }

    private void populateTotalRevenueDetails(CustomerBooking customerBooking, CustomerBookingResponse customerBookingResponse) {
        BigDecimal totalRevenue = BigDecimal.ZERO;
        if (customerBooking != null && customerBooking.getBookingCharges() != null) {
            totalRevenue = customerBooking.getBookingCharges().stream()
                    .filter(Objects::nonNull)
                    .map(c -> c.getLocalSellAmount() != null ? c.getLocalSellAmount() : BigDecimal.ZERO)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
        }
        customerBookingResponse.setTotalRevenue(totalRevenue);
    }

    private void addAllMasterDatas(CustomerBooking customerBooking, CustomerBookingResponse customerBookingResponse) {
        customerBookingResponse.setMasterData(masterDataUtils.addMasterData(customerBookingResponse, CustomerBooking.class));
        if (!Objects.isNull(customerBookingResponse.getCarrierDetails()))
            customerBookingResponse.getCarrierDetails().setMasterData(masterDataUtils.addMasterData(customerBookingResponse.getCarrierDetails(), CarrierDetails.class));
        if (!Objects.isNull(customerBookingResponse.getRoutingList()))
            customerBookingResponse.getRoutingList().forEach(r -> r.setMasterData(masterDataUtils.addMasterData(r, Routings.class)));
        if (!Objects.isNull(customerBookingResponse.getContainersList()))
            customerBookingResponse.getContainersList().forEach(c -> c.setMasterData(masterDataUtils.addMasterData(c, Containers.class)));
        if (!Objects.isNull(customerBookingResponse.getPackingList()))
            customerBookingResponse.getPackingList().forEach(c -> c.setMasterData(masterDataUtils.addMasterData(c, Packing.class)));
    }

    private void addAllUnlocationDatas(CustomerBooking customerBooking, CustomerBookingResponse customerBookingResponse) {
        if (!Objects.isNull(customerBookingResponse.getCarrierDetails()))
            customerBookingResponse.getCarrierDetails().setUnlocationData(masterDataUtils.addUnlocationData(customerBookingResponse.getCarrierDetails(), CarrierDetails.class, EntityTransferConstants.LOCATION_SERVICE_GUID));
        if (!Objects.isNull(customerBookingResponse.getRoutingList()))
            customerBookingResponse.getRoutingList().forEach(r -> r.setUnlocationData(masterDataUtils.addUnlocationData(r, Routings.class, EntityTransferConstants.LOCATION_SERVICE_GUID)));
    }

    private void addDedicatedMasterData(CustomerBooking customerBooking, CustomerBookingResponse customerBookingResponse) {
        if (!Objects.isNull(customerBookingResponse.getCarrierDetails())) {
            customerBookingResponse.getCarrierDetails().setCarrierMasterData(masterDataUtils.carrierMasterData(customerBookingResponse.getCarrierDetails(), CarrierDetails.class));
            customerBookingResponse.getCarrierDetails().setVesselsMasterData(masterDataUtils.vesselsMasterData(customerBookingResponse.getCarrierDetails(), CarrierDetails.class));
        }
        if (!Objects.isNull(customerBookingResponse.getContainersList())) {
            customerBookingResponse.getContainersList().forEach(r -> r.setCommodityTypeData(masterDataUtils.commodityMasterData(r, Containers.class)));
            customerBookingResponse.getContainersList().forEach(r -> r.setContainerCodeData(masterDataUtils.containerCodeMasterData(r, Containers.class)));
        }
        if (!Objects.isNull(customerBookingResponse.getRoutingList()))
            customerBookingResponse.getRoutingList().forEach(r -> r.setCarrierMasterData(masterDataUtils.carrierMasterData(r, Routings.class)));
        if (!Objects.isNull(customerBookingResponse.getPackingList()))
            customerBookingResponse.getPackingList().forEach(r -> r.setCommodityTypeData(masterDataUtils.commodityMasterData(r, Packing.class)));
        if (!Objects.isNull(customerBookingResponse.getBookingCharges()))
            customerBookingResponse.getBookingCharges().forEach(c -> c.setChargeTypeMasterData(masterDataUtils.chargeTypeMasterData(c, BookingCharges.class)));
    }

}
