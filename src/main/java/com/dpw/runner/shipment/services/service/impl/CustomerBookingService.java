package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.ICRPServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.INPMServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.IPlatformServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
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
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingSource;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.exception.exceptions.V1ServiceException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.nimbusds.jose.util.Pair;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
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
    private IV1Service v1Service;

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
    private IPlatformServiceAdapter platformServiceAdapter;
    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private INPMServiceAdapter npmService;

    @Autowired
    UserContext userContext;

    @Autowired
    private ICRPServiceAdapter crpServiceAdapter;

    private static final Map<String, String> loadTypeMap = Map.of("SEA", "LCL", "AIR", "LSE");

    private Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("customerOrgCode", RunnerEntityMapping.builder().tableName("customer").dataType(String.class).fieldName("orgCode").build()),
            Map.entry("consignerOrgCode", RunnerEntityMapping.builder().tableName("consigner").dataType(String.class).fieldName("orgCode").build()),
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
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {

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
            if (!customerBooking.getIsPlatformBookingCreated() && !customerBooking.getBookingCharges().isEmpty()) {
                platformServiceAdapter.createAtPlatform(createPlatformCreateRequest(customerBooking));
                customerBooking.setIsPlatformBookingCreated(true);
                customerBookingDao.save(customerBooking);
            } else if (customerBooking.getIsPlatformBookingCreated() != null && customerBooking.getIsPlatformBookingCreated()) {
                platformServiceAdapter.updateAtPlaform(createPlatformUpdateRequest(customerBooking));
            }
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RuntimeException(e);
        }

        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(customerBooking, CustomerBookingResponse.class));
    }

    private void createEntities(CustomerBooking customerBooking, CustomerBookingRequest request) throws Exception {
        if (customerBooking.getIsPlatformBookingCreated() == null) {
            customerBooking.setIsPlatformBookingCreated(false);
        }
        if (customerBooking.getBookingNumber() == null) {
            customerBooking.setBookingNumber(generateBookingNumber());
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
    }

    private CommonRequestModel createPlatformUpdateRequest(@NonNull final CustomerBooking customerBooking) {
        var carrierDetails = Optional.ofNullable(customerBooking.getCarrierDetails());
        PlatformUpdateRequest platformUpdateRequest = PlatformUpdateRequest.builder()
                .booking_reference_code(customerBooking.getBookingNumber())
                .origin_code(carrierDetails.map(c -> c.getOrigin()).orElse(null))
                .destination_code(carrierDetails.map(c -> c.getDestination()).orElse(null))
                .load(createLoad(customerBooking))
                .route(createRoute(customerBooking))
                .charges(createCharges(customerBooking))
                .carrier_code(carrierDetails.map(c -> c.getJourneyNumber()).orElse(null))
                .air_carrier_details(null)
                .status(customerBooking.getBookingStatus().getDescription())
                .pickup_date(null)
                .eta(carrierDetails.map(c -> c.getEta()).orElse(null))
                .ets(carrierDetails.map(c -> c.getEtd()).orElse(null))
                .build();
        return CommonRequestModel.builder().data(platformUpdateRequest).build();
    }

    private CommonRequestModel createPlatformCreateRequest(CustomerBooking customerBooking) {
        var carrierDetails = Optional.ofNullable(customerBooking.getCarrierDetails());
        PlatformCreateRequest platformCreateRequest = PlatformCreateRequest.builder()
                .booking_ref_code(customerBooking.getBookingNumber())
                .origin_code(carrierDetails.map(c -> c.getOrigin()).orElse(null))
                .destination_code(carrierDetails.map(c -> c.getDestination()).orElse(null))
                .pol(carrierDetails.map(c -> c.getOriginPort()).orElse(null))
                .pod(carrierDetails.map(c -> c.getDestinationPort()).orElse(null))
                .contract_id(customerBooking.getContractId().toString())
                .created_at(customerBooking.getCreatedAt())
                .customer_org_id(customerBooking.getCustomer().getOrgCode())
                .load(createLoad(customerBooking))
                .route(createRoute(customerBooking))
                .charges(createCharges(customerBooking))
                .business_code(customerBooking.getBusinessCode())
                .bill_to_party(createOrgRequest(customerBooking.getCustomer()))
                .build();
        return CommonRequestModel.builder().data(platformCreateRequest).build();
    }

    private OrgRequest createOrgRequest(Parties parties) {
        return OrgRequest.builder()
                .org_id(parties.getOrgCode())
                .office_id(parties.getAddressCode())
                .org_name(String.valueOf(parties.getOrgData().get(PartiesConstants.FULLNAME)))
                .build();
    }

    private List<ChargesRequest> createCharges(CustomerBooking customerBooking) {
        var bookingCharges = customerBooking.getBookingCharges();
        List<ChargesRequest> charges = new ArrayList<>();
        bookingCharges.forEach(
                bookingCharge -> {
                    charges.add(
                            ChargesRequest.builder()
                                    .load_uuid(bookingCharge.getContainersList().stream().map(c -> c.getGuid()).collect(Collectors.toList()))
                                    .charge_group("ORIGIN_CHARGES") //TODO - fetch from charge type master
                                    .charge_code(bookingCharge.getChargeType())
                                    .charge_code_desc(bookingCharge.getChargeType()) //TODO - Fetch charge type master and set charge_code_desc = description
                                    .base_charge_value(bookingCharge.getLocalSellAmount())
                                    .charge_value(bookingCharge.getOverseasSellAmount())
                                    .base_currency(bookingCharge.getLocalSellCurrency())
                                    .charge_currency(bookingCharge.getOverseasSellCurrency())
                                    .exchange_rate(bookingCharge.getSellExchange())
                                    .charge_id(bookingCharge.getGuid())
                                    .taxes(null) // optional
                                    .build()
                    );
                }
        );

        return charges;
    }

    private RouteRequest createRoute(CustomerBooking customerBooking) {
        List<RouteLegRequest> legRequestList = new ArrayList<>();
        List<Routings> routingsList = customerBooking.getRoutingList();

        for (int counter = 0; counter < routingsList.size(); counter++) {
            legRequestList.add(RouteLegRequest.builder()
                    .destination_code(routingsList.get(counter).getPod())
                    .origin_code(routingsList.get(counter).getPol())
                    .order(routingsList.get(counter).getLeg())
                    .transport_mode(routingsList.get(counter).getMode())
                    .build());
        }

        return RouteRequest.builder()
                .legs(legRequestList)
                .build();
    }

    private List<LoadRequest> createLoad(final CustomerBooking customerBooking) {
        List<LoadRequest> loadRequests = new ArrayList<>();
        //Container -> FCL
        if (customerBooking.getCargoType().equals("FCL")) {
            List<Containers> containers = customerBooking.getContainersList();
            containers.forEach(container -> {
                loadRequests.add(LoadRequest.builder()
                        .load_uuid(container.getGuid())
                        .load_type(customerBooking.getCargoType())
                        .container_type_code(container.getContainerCode())
                        .pkg_type(null)
                        .is_package(false)
                        .weight(container.getGrossWeight())
                        .quantity(container.getContainerCount())
                        .weight_uom(container.getGrossWeightUnit())
                        .quantity_uom("unit")
                        .volume(container.getGrossVolume())
                        .volume_uom(container.getGrossVolumeUnit())
                        .dimensions(null) // Resolved
                        .build());
            });
        }

        if (customerBooking.getCargoType().equals("LCL") || customerBooking.getCargoType().equals("LSE")) {
            List<Packing> packings = customerBooking.getPackingList();
            packings.forEach(packing -> {
                loadRequests.add(LoadRequest.builder()
                        .load_uuid(packing.getGuid())
                        .load_type(customerBooking.getCargoType())
                        .container_type_code(null)
                        .pkg_type(packing.getPacksType())
                        .is_package(true)
                        .weight(packing.getWeight())
                        .quantity(Long.valueOf(packing.getPacks()))
                        .weight_uom(packing.getWeightUnit())
                        .quantity_uom("unit")
                        .volume(packing.getVolume())
                        .volume_uom(packing.getVolumeUnit())
                        .dimensions(getDimension(customerBooking, packing))
                        .build());
            });
        }

        return loadRequests;
    }

    private DimensionDTO getDimension(CustomerBooking booking, Packing packing) {
        if (booking.getCargoType().equals("LCL") || booking.getCargoType().equals("LSE"))
            return DimensionDTO.builder()
                    .length(packing.getLength())
                    .width(packing.getWidth())
                    .height(packing.getHeight())
                    .uom(packing.getLengthUnit())
                    .build();
        return null;
    }

    @Override
    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
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
        CustomerBooking customerBooking = jsonHelper.convertValue(request, CustomerBooking.class);
        customerBooking.setCreatedAt(oldEntity.get().getCreatedAt());
        customerBooking.setCreatedBy(oldEntity.get().getCreatedBy());
        customerBooking.setSource(BookingSource.Runner);

        // NPM update contract
        if (oldEntity.get().getBookingCharges() != null) {
            npmContractUpdate(customerBooking, true, oldEntity.get());
        } else {
            npmContractUpdate(customerBooking, false, oldEntity.get());
        }

        try {
            this.updateEntities(customerBooking, request);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RuntimeException(e);
        }
        if (!customerBooking.getIsPlatformBookingCreated() && !customerBooking.getBookingCharges().isEmpty()) {
            try {
                platformServiceAdapter.createAtPlatform(createPlatformCreateRequest(customerBooking));
                customerBooking.setIsPlatformBookingCreated(true);
                customerBookingDao.save(customerBooking);
            } catch (Exception e) {
                log.error("ERROR creating in Platform in Update Booking API, ERROR : " + e.getMessage());
                throw new RuntimeException(e);
            }
        } else if (customerBooking.getIsPlatformBookingCreated() != null && customerBooking.getIsPlatformBookingCreated()) {
            try {
                platformServiceAdapter.updateAtPlaform(createPlatformUpdateRequest(customerBooking));
            } catch (Exception e) {
                log.error("ERROR updating in Platform in Update Booking API, ERROR : " + e.getMessage());
                throw new RuntimeException(e);
            }
        }

        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(customerBooking, CustomerBookingResponse.class));
    }

    private void updateEntities(CustomerBooking customerBooking, CustomerBookingRequest request) throws Exception {
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
        if (customerBooking.getBookingStatus().equals(BookingStatus.READY_FOR_SHIPMENT)) {
            var response = v1Service.createBooking(customerBooking);
            if (!response.getStatusCode().equals(HttpStatus.OK))
                throw new V1ServiceException("Cannot create booking in v1 for the customerBooking guid: " + customerBooking.getGuid());
        }
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
    @Transactional
    public ResponseEntity<?> platformCreateBooking(CommonRequestModel commonRequestModel) {
        PlatformToRunnerCustomerBookingRequest request = (PlatformToRunnerCustomerBookingRequest) commonRequestModel.getData();
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
        customerBookingRequest.setSource(BookingSource.Platform);
        if (customerBooking.isEmpty()) {
            this.createPlatformBooking(customerBookingRequest);
        } else {
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

            this.updatePlatformBooking(customerBookingRequest);
        }

        return ResponseHelper.buildSuccessResponse(platformResponse);
    }

    private void setOrgAndAddressToParties(PlatformToRunnerCustomerBookingRequest request) {
        if (request.getCustomer() != null) {
            String orgCode = request.getCustomer().getOrgCode();
            String addressCode = request.getCustomer().getAddressCode();
            transformOrgAndAddressPayload(request.getCustomer(), addressCode, orgCode);
        }
        if (request.isConsignorFreeText() && request.getConsignor() != null) {
            transformOrgAndAddressToRawData(request.getConsignor());
        }
        if (request.isConsigneeFreeText() && request.getConsignee() != null) {
            transformOrgAndAddressToRawData(request.getConsignee());
        }
        if (request.isNotifyPartyFreeText() && request.getNotifyParty() != null) {
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
            ResponseEntity<CRPRetrieveResponse> crpResponse = (ResponseEntity<CRPRetrieveResponse>) crpServiceAdapter.retrieveCRPService(CommonRequestModel.buildRequest(retrieveRequest));
            response = modelMapper.map(crpResponse.getBody(), CRPRetrieveResponse.class);
        } catch (Exception e) {
            log.error("CRP Retrieve failed due to: " + e.getMessage());
            throw new RuntimeException(e);
        }
        if (response == null) {
            log.error("No organization exist in CRP with OrgCode: " + orgCode);
            throw new DataRetrievalFailureException("No organization exist in CRP with OrgCode: " + orgCode);
        }

        Map<String, Object> orgData = new HashMap<>();
        Map<String, Object> addressData = new HashMap<>();
        CRPRetrieveResponse.CRPAddressDetails crpAddressDetails = new CRPRetrieveResponse.CRPAddressDetails();
        List<CRPRetrieveResponse.CRPAddressDetails> crpAddressDetailsList = response.getCompanyOfficeDetails().stream().filter(x -> x.getOfficeReference().equals(addressCode)).collect(Collectors.toList());
        if (!crpAddressDetailsList.isEmpty())
            crpAddressDetails = crpAddressDetailsList.get(0);

        String fusionSiteIdentifier = null;
        String billableFlag = "";
        List<CRPRetrieveResponse.CompanyCodeIssuerDetails> companyCodeIssuerDetailsList = response.getCompanyCodeIssuerDetails().stream().filter(x -> x.getIdentifierValue().equals(addressCode)).collect(Collectors.toList());
        if (!companyCodeIssuerDetailsList.isEmpty()) {
            var fusionSiteIdList = companyCodeIssuerDetailsList.stream().filter(x -> x.getIdentifierCodeType().equals(PartiesConstants.FUSION_SITE_ID)).collect(Collectors.toList());
            var billableFlagList = companyCodeIssuerDetailsList.stream().filter(x -> x.getIdentifierCodeType().equals(PartiesConstants.BILLABLE_FLAG)).collect(Collectors.toList());
            if (!fusionSiteIdList.isEmpty())
                fusionSiteIdentifier = fusionSiteIdList.get(0).getIdentifierCode();
            if (!billableFlagList.isEmpty())
                billableFlag = billableFlagList.get(0).getIdentifierCode();
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
        orgData.put(PartiesConstants.RECEIVABLES, billableFlag.equals(CustomerBookingConstants.YES));

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

    private CustomerBookingResponse updatePlatformBooking(CustomerBookingRequest request) {
        CustomerBooking customerBooking = jsonHelper.convertValue(request, CustomerBooking.class);
        customerBooking.setSource(BookingSource.Platform);
        try {
            this.updateEntities(customerBooking, request);
        } catch (Exception e) {
            log.error(e.getMessage());
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
        return jsonHelper.convertValue(customerBooking, CustomerBookingResponse.class);
    }

    private CustomerBookingResponse createPlatformBooking(CustomerBookingRequest request) {
        if (request == null) {
            log.error("Request is null for Customer Booking Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        CustomerBooking customerBooking = jsonHelper.convertValue(request, CustomerBooking.class);
        customerBooking.setSource(BookingSource.Platform);
        try {
            createEntities(customerBooking, request);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RuntimeException(e);
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
        return responseList;
    }

    private void npmContractUpdate(CustomerBooking customerBooking, Boolean isAlteration, CustomerBooking oldEntity) {
        if (customerBooking.getTransportType().equals(Constants.TRANSPORT_MODE_SEA) && customerBooking.getCargoType().equals(Constants.CARGO_TYPE_FCL) &&
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

                        } else if (!cont.getCommodityCode().equals(idVsContainerMap.get(cont.getId()).getCommodityCode())) {
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
            if (!idVsContainerMap.isEmpty()) {
                idVsContainerMap.values().forEach(cont -> {
                    loadInfoRequestList.add(containerLoadConstruct(cont, CustomerBookingConstants.ADD, cont.getContainerCount()));
                });
            }
        } else if (isCancelled && oldEntity != null && oldEntity.getContainersList() != null) {
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
                .product_category_code(container.getCommodityCode())
                .hazardous_info(HazardousInfoRequest.builder().is_hazardous(false).build())
                .build());
        loadInfoRequest.setLoad_attributes(LoadAttributesRequest.builder()
                .quantity(quantity)
                .quantity_uom(CustomerBookingConstants.UNIT)
                .build());
        return loadInfoRequest;
    }

    private void npmContractUpdatePaylaod(CustomerBooking customerBooking, Boolean isAlteration, List<LoadInfoRequest> loadInfoRequestList) {
        String contractStatus = null;
        if (customerBooking.getContractStatus().equals(CustomerBookingConstants.SINGLE_USAGE) && customerBooking.getBookingStatus() == BookingStatus.CANCELLED) {
            contractStatus = CustomerBookingConstants.ENABLED;
        } else if (customerBooking.getContractStatus().equals(CustomerBookingConstants.SINGLE_USAGE)) {
            contractStatus = CustomerBookingConstants.DISABLED;
        }

        UpdateContractRequest updateContractRequest = UpdateContractRequest.builder()
                .contract_id(customerBooking.getContractId())
                .contract_status(contractStatus)
                .source(CustomerBookingConstants.RUNNER)
                .source_type(CustomerBookingConstants.RUNNER)
                .loads_info(loadInfoRequestList)
                .is_alteration(isAlteration)
                .build();

        try {
            npmService.updateContracts(CommonRequestModel.buildRequest(updateContractRequest));
        } catch (Exception e) {
            log.error("Update Contract Request failed due to: " + e.getMessage());
            throw new RuntimeException(e);
        }
    }

    private String generateBookingNumber() {
        return "SROF" + "-" + getRandomNumberString(6) + "-" + getRandomNumberString(5);
    }

    public static String getRandomNumberString(int digit) {
        Random rnd = new Random();
        int number = 0;
        if (digit == 6)
            number = rnd.nextInt(999999);
        else
            number = rnd.nextInt(99999);
        if (digit == 6)
            return String.format("%06d", number);
        return String.format("%05d", number);
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

    private void addAllMasterDatas(CustomerBooking customerBooking, CustomerBookingResponse customerBookingResponse) {
        customerBookingResponse.setMasterData(masterDataUtils.addMasterData(customerBookingResponse, CustomerBooking.class));
        customerBookingResponse.getCarrierDetails().setMasterData(masterDataUtils.addMasterData(customerBookingResponse.getCarrierDetails(), CarrierDetails.class));
        customerBookingResponse.getRoutingList().forEach(r -> r.setMasterData(masterDataUtils.addMasterData(r, Routings.class)));
        customerBookingResponse.getContainersList().forEach(c -> c.setMasterData(masterDataUtils.addMasterData(c, Containers.class)));
        customerBookingResponse.getPackingList().forEach(c -> c.setMasterData(masterDataUtils.addMasterData(c, Packing.class)));
    }

    private void addAllUnlocationDatas(CustomerBooking customerBooking, CustomerBookingResponse customerBookingResponse) {
        customerBookingResponse.getCarrierDetails().setUnlocationData(masterDataUtils.addUnlocationData(customerBookingResponse.getCarrierDetails(), CarrierDetails.class, EntityTransferConstants.LOCATION_SERVICE_GUID));
        customerBookingResponse.getRoutingList().forEach(r -> r.setUnlocationData(masterDataUtils.addUnlocationData(r, Routings.class, EntityTransferConstants.LOCATION_SERVICE_GUID)));
    }

    private void addDedicatedMasterData(CustomerBooking customerBooking, CustomerBookingResponse customerBookingResponse) {
        customerBookingResponse.getCarrierDetails().setCarrierMasterData(masterDataUtils.carrierMasterData(customerBookingResponse.getCarrierDetails(), CarrierDetails.class));
        customerBookingResponse.getRoutingList().forEach(r -> r.setCarrierMasterData(masterDataUtils.carrierMasterData(r, Routings.class)));
        customerBookingResponse.getContainersList().forEach(r -> r.setCommodityTypeData(masterDataUtils.commodityMasterData(r, Containers.class)));
        customerBookingResponse.getPackingList().forEach(r -> r.setCommodityTypeData(masterDataUtils.commodityMasterData(r, Packing.class)));
        customerBookingResponse.getContainersList().forEach(r -> r.setContainerCodeData(masterDataUtils.containerCodeMasterData(r, Containers.class)));
        customerBookingResponse.getCarrierDetails().setVesselsMasterData(masterDataUtils.vesselsMasterData(customerBookingResponse.getCarrierDetails(), CarrierDetails.class));
        customerBookingResponse.getBookingCharges().forEach(c -> c.setChargeTypeMasterData(masterDataUtils.chargeTypeMasterData(c, BookingCharges.class)));
    }


}
