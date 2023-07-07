package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.repository.interfaces.*;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
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
import java.util.concurrent.ExecutionException;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ShipmentService implements IShipmentService {

    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private ICarrierDao carrierDao;
    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IPackingService packingService;

    @Autowired
    private IBookingCarriageService bookingCarriageService;

    @Autowired
    private ICarrierDetailService carrierDetailService;

    @Autowired
    private IELDetailsService elDetailsService;

    @Autowired
    private IEventService eventService;

    @Autowired
    private IFileRepoService fileRepoService;

    @Autowired
    private IJobService jobService;

    @Autowired
    private INotesService notesService;

    @Autowired
    private IPickupDeliveryDetailsService pickupDeliveryDetailsService;

    @Autowired
    private IReferenceNumbersService referenceNumbersService;

    @Autowired
    private IRoutingsService routingsService;

    @Autowired
    private IServiceDetailsService serviceDetailsService;

    @Autowired
    private IAdditionalDetailService additionalDetailService;

    @Autowired
    private IContainerService containerService;

    @Autowired
    private IPartiesDetailsService partiesDetailsService;

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

            shipmentDetail = shipmentDao.save(shipmentDetail);
            /**
             * Parties Details*
             */
            List<Parties> partiesDetails = createParties(shipmentDetail);
            shipmentDetail.setParties(partiesDetails);
            response.add(shipmentDetail);
        }

        return response;
    }

    @Override
    public ResponseEntity<?> fetchShipments(CommonRequestModel commonRequestModel) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();

        Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
        return ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                shipmentDetailsPage.getTotalPages(),
                shipmentDetailsPage.getTotalElements());
    }

    private static ShipmentDetailsResponse convertEntityToDto(ShipmentDetails shipmentDetails) {
        ShipmentDetailsResponse response = new ShipmentDetailsResponse();
        response.setId(shipmentDetails.getId());
        response.setGuid(shipmentDetails.getGuid());
        response.setParties(shipmentDetails.getParties());
        response.setGoodsDescription(shipmentDetails.getGoodsDescription());
        response.setAdditionalTerms(shipmentDetails.getAdditionalTerms());
        response.setAssignedTo(shipmentDetails.getAssignedTo());
        response.setIsDomestic(shipmentDetails.getIsDomestic());
        response.setShipmentId(shipmentDetails.getShipmentId());
        response.setIncoterms(shipmentDetails.getIncoterms());
        response.setPaymentTerms(shipmentDetails.getPaymentTerms());
        response.setSalesAgent(shipmentDetails.getSalesAgent());
        response.setConsolRef(shipmentDetails.getConsolRef());
        response.setBookingReference(shipmentDetails.getBookingReference());
        response.setMasterBill(shipmentDetails.getMasterBill());
        response.setServiceType(shipmentDetails.getServiceType());
        response.setJobType(shipmentDetails.getJobType());
        response.setSource(shipmentDetails.getSource());
        response.setStatus(shipmentDetails.getStatus());
        response.setShipmentType(shipmentDetails.getShipmentType());
        response.setDirection(shipmentDetails.getDirection());
        response.setTransportMode(shipmentDetails.getTransportMode());
        response.setHouseBill(shipmentDetails.getHouseBill());
        response.setCarrierDetails(shipmentDetails.getCarrierDetails());


        return response;
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ShipmentDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(shipmentDetail -> {
            responseList.add(jsonHelper.convertValue(shipmentDetail, ShipmentDetailsResponse.class));
        });
        return responseList;
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
        ShipmentRequest request = null;
        request = (ShipmentRequest) commonRequestModel.getData();
        // TODO- implement validator
        ShipmentDetails shipmentDetails = jsonHelper.convertValue(request, ShipmentDetails.class);
        shipmentDetails = shipmentDao.save(shipmentDetails);
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(shipmentDetails, ShipmentDetailsResponse.class));
    }

    @Override
    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) throws Exception {
        ShipmentRequest request = (ShipmentRequest) commonRequestModel.getData();
        // TODO- implement Validation logic
        long id = request.getId();
        Optional<ShipmentDetails> oldEntity = shipmentDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Shipment Details is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ShipmentDetails entity = jsonHelper.convertValue(request, ShipmentDetails.class);
        entity.setId(oldEntity.get().getId());
        entity = shipmentDao.save(entity);
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(entity, ShipmentDetailsResponse.class));
    }

    @Transactional
    public ResponseEntity<?> completeUpdate(CommonRequestModel commonRequestModel) throws Exception {

        CompleteShipmentRequest completeShipmentRequest = (CompleteShipmentRequest) commonRequestModel.getData();
        ShipmentRequest request = (ShipmentRequest) completeShipmentRequest.getShipmentRequest();

        List<BookingCarriageRequest> bookingCarriageRequestList = completeShipmentRequest.getBookingCarriageRequest();
        List<PackingRequest> packingRequestList = completeShipmentRequest.getPackingRequest();
//        List<AdditionalDetailRequest> additionalDetailRequestList = Arrays.asList(request.getAdditionalDetailRequest());
        List<ContainerRequest> containerRequestList = completeShipmentRequest.getContainerRequest();
        List<ELDetailsRequest> elDetailsRequestList = completeShipmentRequest.getElDetailsRequest();
        List<EventsRequest> eventsRequestList = completeShipmentRequest.getEventsRequest();
        List<FileRepoRequest> fileRepoRequestList = completeShipmentRequest.getFileRepoRequest();
        List<JobRequest> jobRequestList = completeShipmentRequest.getJobRequest();
        List<NotesRequest> notesRequestList = completeShipmentRequest.getNotesRequest();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = completeShipmentRequest.getReferenceNumbersRequest();
        List<RoutingsRequest> routingsRequestList = completeShipmentRequest.getRoutingsRequest();
        List<ServiceDetailsRequest> serviceDetailsRequestList = completeShipmentRequest.getServiceDetailsRequest();
//        List<CarrierDetailRequest> carrierDetailRequestList = Arrays.asList(request.getCarrierDetailRequest());
        List<PickupDeliveryDetailsRequest> pickupDeliveryDetailsRequestList = completeShipmentRequest.getPickupDeliveryDetailsRequest();
        List<PartiesRequest> partiesRequestList = completeShipmentRequest.getPartiesRequest();

        // TODO- implement Validation logic
        long id = request.getId();
        Optional<ShipmentDetails> oldEntity = shipmentDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Shipment Details is null for Id {}", request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try {
            ResponseEntity<?> updatedBookingCarriages = bookingCarriageService.updateEntityFromShipment(CommonRequestModel.buildRequest(bookingCarriageRequestList), id);
            ResponseEntity<?> updatedPackings = packingService.updateEntityFromShipment(CommonRequestModel.buildRequest(packingRequestList), id);
//            ResponseEntity<?> updatedAdditionalDetails = additionalDetailService.updateEntityFromShipment(CommonRequestModel.buildRequest(additionalDetailRequestList), id);
            ResponseEntity<?> updatedContainers = containerService.updateEntityFromShipment(CommonRequestModel.buildRequest(containerRequestList), id);
            ResponseEntity<?> updatedELDetails = elDetailsService.updateEntityFromShipment(CommonRequestModel.buildRequest(elDetailsRequestList), id);
            ResponseEntity<?> updatedEvents = eventService.updateEntityFromShipment(CommonRequestModel.buildRequest(eventsRequestList), id);
            ResponseEntity<?> updatedFileRepos = fileRepoService.updateEntityFromShipment(CommonRequestModel.buildRequest(fileRepoRequestList), id);
            ResponseEntity<?> updatedJobs = jobService.updateEntityFromShipment(CommonRequestModel.buildRequest(jobRequestList), id);
            ResponseEntity<?> updatedNotes = notesService.updateEntityFromShipment(CommonRequestModel.buildRequest(notesRequestList), id);
            ResponseEntity<?> updatedReferenceNumbers = referenceNumbersService.updateEntityFromShipment(CommonRequestModel.buildRequest(referenceNumbersRequestList), id);
            ResponseEntity<?> updatedRoutings = routingsService.updateEntityFromShipment(CommonRequestModel.buildRequest(routingsRequestList), id);
            ResponseEntity<?> updatedServiceDetails = serviceDetailsService.updateEntityFromShipment(CommonRequestModel.buildRequest(serviceDetailsRequestList), id);
//            ResponseEntity<?> updatedCarrierDetails = carrierDetailService.updateEntityFromShipment(CommonRequestModel.buildRequest(carrierDetailRequestList), id);
            ResponseEntity<?> updatedPickupDeliveryDetails = pickupDeliveryDetailsService.updateEntityFromShipment(CommonRequestModel.buildRequest(pickupDeliveryDetailsRequestList), id);
            ResponseEntity<?> updatedPartiesDetails = partiesDetailsService.updateEntityFromShipment(CommonRequestModel.buildRequest(partiesRequestList), id);

            ShipmentDetails entity = jsonHelper.convertValue(request, ShipmentDetails.class);
            entity.setId(oldEntity.get().getId());
            entity = shipmentDao.save(entity);
            CompleteShipmentResponse response = CompleteShipmentResponse.builder().
                    bookingCarriages(getResponse(updatedBookingCarriages)).
                    packings(getResponse(updatedPackings)).
//                    additionalDetails(getResponse(updatedAdditionalDetails)).
                    containers(getResponse(updatedContainers)).
                    elDetails(getResponse(updatedELDetails)).
                    events(getResponse(updatedEvents)).
                    fileRepo(getResponse(updatedFileRepos)).
                    job(getResponse(updatedJobs)).
                    notes(getResponse(updatedNotes)).
                    referenceNumbers(getResponse(updatedReferenceNumbers)).
                    routings(getResponse(updatedRoutings)).
                    serviceDetails(getResponse(updatedServiceDetails)).
//                    carrierDetails(getResponse(updatedCarrierDetails)).
                    pickupDeliveryDetails(getResponse(updatedPickupDeliveryDetails)).
                    parties(getResponse(updatedPartiesDetails)).

                    shipment(convertEntityToDto(entity)).build();
            return ResponseHelper.buildSuccessResponse(response);
        } catch (ExecutionException e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();

            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(shipmentDetailsPage.getContent()),
                    shipmentDetailsPage.getTotalPages(),
                    shipmentDetailsPage.getTotalElements());
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

            Pair<Specification<ShipmentDetails>, Pageable> tuple = fetchData(request, ShipmentDetails.class, tableNames);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(tuple.getLeft(), tuple.getRight());
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
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            shipmentDao.delete(shipmentDetails.get());
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
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            ShipmentDetailsResponse response = jsonHelper.convertValue(shipmentDetails.get(), ShipmentDetailsResponse.class);
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
            long id = request.getId();
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(id);
            if (!shipmentDetails.isPresent()) {
                log.debug("Shipment Details is null for Id {}", request.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            ShipmentDetailsResponse response = jsonHelper.convertValue(shipmentDetails.get(), ShipmentDetailsResponse.class);
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(response));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    public ResponseEntity<?> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException {
        try{
        // create common list request for shipment id
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        long id = request.getId();
        CommonRequestModel commonListRequestModel = CommonRequestModel.buildRequest(constructListCommonRequest("shipmentId",id,"=" ));
        CommonRequestModel commonListRequestModelbyEntityId = CommonRequestModel.buildRequest(constructListCommonRequest("entityId",id,"=" ));

        CompletableFuture<ResponseEntity<?>> shipmentsFuture = retrieveByIdAsync(commonRequestModel);
        CompletableFuture<ResponseEntity<?>> packingsFuture = packingService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> additionalDetailsFuture = additionalDetailService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> bookingCarriagesFuture = bookingCarriageService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> containersFuture = containerService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> elDetailsFuture = elDetailsService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> eventsFuture = eventService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> fileRepoFuture = fileRepoService.listAsync(commonListRequestModelbyEntityId);
        CompletableFuture<ResponseEntity<?>> jobsFuture = jobService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> notesFuture = notesService.listAsync(commonListRequestModelbyEntityId);
        CompletableFuture<ResponseEntity<?>> referenceNumbersFuture = referenceNumbersService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> routingsFuture = routingsService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> serviceDetailsFuture = serviceDetailsService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> carrierDetailsFuture = carrierDetailService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> pickupDeliveryDetailsFuture =pickupDeliveryDetailsService.listAsync(commonListRequestModel);
        CompletableFuture<ResponseEntity<?>> partiesFuture = partiesDetailsService.listAsync(commonListRequestModelbyEntityId);

        CompletableFuture.allOf(shipmentsFuture, packingsFuture, additionalDetailsFuture, bookingCarriagesFuture, containersFuture,
                elDetailsFuture, eventsFuture, fileRepoFuture, jobsFuture, notesFuture, referenceNumbersFuture, routingsFuture,
                serviceDetailsFuture, pickupDeliveryDetailsFuture, partiesFuture).join();

        var response = new CompleteShipmentResponse();
        var res = (RunnerResponse<ShipmentDetailsResponse>) shipmentsFuture.get().getBody();

        response.setShipment(res.getData());
        response.setPackings(getResponse(packingsFuture));
        response.setAdditionalDetails(getResponse(additionalDetailsFuture));
        response.setBookingCarriages(getResponse(bookingCarriagesFuture));
        response.setContainers(getResponse(containersFuture));
        response.setElDetails(getResponse(elDetailsFuture));
        response.setEvents(getResponse( eventsFuture));
        response.setFileRepo(getResponse(fileRepoFuture));
        response.setJob(getResponse(jobsFuture));
        response.setNotes(getResponse(notesFuture));
        response.setReferenceNumbers(getResponse(referenceNumbersFuture));
        response.setRoutings(getResponse( routingsFuture));
        response.setServiceDetails(getResponse( serviceDetailsFuture));
        response.setPickupDeliveryDetails(getResponse( pickupDeliveryDetailsFuture));
        response.setParties(getResponse(partiesFuture));

        return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }


    private <T extends IRunnerResponse> List<T> getResponse(CompletableFuture<ResponseEntity<?>> responseEntity) throws ExecutionException, InterruptedException {
        var runnerListResponse = (RunnerListResponse<T>) responseEntity.get().getBody();
        return (List<T>) runnerListResponse.getData();
    }

    private <T extends IRunnerResponse> List<T> getResponse(ResponseEntity<?> responseEntity) throws ExecutionException, InterruptedException {
        var runnerListResponse = (RunnerListResponse<T>) responseEntity.getBody();
        return (List<T>) runnerListResponse.getData();
    }

    private Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry("type", RunnerEntityMapping.builder().tableName("parties").dataType(String.class).build()),
            Map.entry("orgId", RunnerEntityMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("houseBill", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("hblType", RunnerEntityMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("transportMode", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("releaseType", RunnerEntityMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("deliveryMode", RunnerEntityMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("direction", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("shipmentType", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("status", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).build()),
            Map.entry("source", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("jobType", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("serviceType", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("masterBill", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("bookingReference", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("consolRef", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("salesAgent", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Long.class).build()),
            Map.entry("paymentTerms", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("incoterms", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("shipmentId", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("isDomestic", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Boolean.class).build()),
            Map.entry("assignedTo", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Integer.class).build()),
            Map.entry("additionalTerms", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("goodsDescription", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(String.class).build()),
            Map.entry("createdAt", RunnerEntityMapping.builder().tableName("ShipmentDetails").dataType(Date.class).build()),
            Map.entry("estimatedPickup", RunnerEntityMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).build()),
            Map.entry("actualPickup", RunnerEntityMapping.builder().tableName("pickupDetails").dataType(LocalDateTime.class).build()),
            Map.entry("estimatedDelivery", RunnerEntityMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).build()),
            Map.entry("requiredBy", RunnerEntityMapping.builder().tableName("deliveryDetails").dataType(LocalDateTime.class).build()),
            Map.entry("addressId", RunnerEntityMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("screeningStatus", RunnerEntityMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("paidPlace", RunnerEntityMapping.builder().tableName("blDetails").dataType(Long.class).build()),
            Map.entry("placeOfIssue", RunnerEntityMapping.builder().tableName("blDetails").dataType(Long.class).build()),
            Map.entry("dateOfIssue", RunnerEntityMapping.builder().tableName("blDetails").dataType(LocalDateTime.class).build()),
            Map.entry("dateOfReceipt", RunnerEntityMapping.builder().tableName("blDetails").dataType(LocalDateTime.class).build()),
            Map.entry("goodsCo", RunnerEntityMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("boeDate", RunnerEntityMapping.builder().tableName("blDetails").dataType(LocalDateTime.class).build()),
            Map.entry("boeNumber", RunnerEntityMapping.builder().tableName("blDetails").dataType(String.class).build()),
            Map.entry("shippingLine", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("vessel", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("voyage", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("origin", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("eta", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("etd", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("ata", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("weight", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("weightUnit", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("volume", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("volumeUnit", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("volumetricWeight", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("volumetricWeightUnit", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("chargable", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("chargeableUnit", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("netWeight", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(BigDecimal.class).build()),
            Map.entry("netWeightUnit", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("noOfPacks", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(Integer.class).build()),
            Map.entry("packsUnit", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(String.class).build()),
            Map.entry("innerPacks", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(Integer.class).build()),
            Map.entry("innerPackUnit", RunnerEntityMapping.builder().tableName("measurementDetails").dataType(String.class).build())
    );

}
