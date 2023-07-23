package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
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
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ConsolidationService implements IConsolidationService {

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private ICarrierDao carrierDao;
    @Autowired
    private IAllocationsDao allocationsDao;
    @Autowired
    private IAchievedQuantitiesDao achievedQuantitiesDao;
    @Autowired
    private IArrivalDepartureDetailsDao arrivalDepartureDetailsDao;
    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IEventDao eventDao;

    @Autowired
    private IFileRepoDao fileRepoDao;

    @Autowired
    private IJobDao jobDao;

    @Autowired
    private INotesDao notesDao;

    @Autowired
    private IReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private IRoutingsDao routingsDao;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private IShipmentDao shipmentDao;

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
            Map.entry("type", RunnerEntityMapping.builder().tableName("parties").dataType(String.class).build()),
            Map.entry("orgCode", RunnerEntityMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("consolidationNumber", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("consolidationType", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("transportMode", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("releaseType", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("deliveryMode", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("containerCategory", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("shipmentType", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("MAWB", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("serviceLevel", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("bookingType", RunnerEntityMapping.builder().tableName("ConsolidationDetails").dataType(String.class).build()),
            Map.entry("addressCode", RunnerEntityMapping.builder().tableName("parties").dataType(Integer.class).build()),
            Map.entry("shippingLine", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("vessel", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("voyage", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("origin", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("destination", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(String.class).build()),
            Map.entry("eta", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("etd", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("ata", RunnerEntityMapping.builder().tableName("carrierDetails").dataType(LocalDateTime.class).build()),
            Map.entry("containerNumber", RunnerEntityMapping.builder().tableName("containers").dataType(String.class).build()),
            Map.entry("containerCode", RunnerEntityMapping.builder().tableName("containers").dataType(String.class).build())
    );

    @Override
    @Transactional
    public List<ConsolidationDetails> createTestConsolidations(Integer count) {
        List<ConsolidationDetails> response = new ArrayList<>();
        /**
         * * * * * * *
         * * * */

        for (int i = 0; i < count; i++) {

            ConsolidationDetails consolidationDetails = createConsolidationData();
            /**
             * Carrier Details*
             */
            CarrierDetails carrierDetail = createCarrier();
            consolidationDetails.setCarrierDetails(carrierDetail);

            consolidationDetails = consolidationDetailsDao.save(consolidationDetails);
        }

        return response;
    }

    @Override
    public ResponseEntity<?> fetchConsolidations(CommonRequestModel commonRequestModel) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();

        Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
        Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
        return ResponseHelper.buildListSuccessResponse(
                convertEntityListToDtoList(consolidationDetailsPage.getContent()),
                consolidationDetailsPage.getTotalPages(),
                consolidationDetailsPage.getTotalElements());
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<ConsolidationDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(consolidationDetails -> {
            responseList.add(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class));
        });
        return responseList;
    }

    private List<Parties> createParties(ConsolidationDetails consolidationDetails) {
        List<Parties> parties = new ArrayList<>();
        int random = new Random().nextInt(100);
        for (String partyType : PARTY_TYPE) {
            Parties party = Parties.builder()
                    .type(partyType).orgCode(generateString(7)).addressCode(generateString(7))
                    .orgData(ORG).addressData(ADDRESS)
                    .entityId(consolidationDetails.getId()).entityType("CONSOLIDATION")
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

    //TODO - Fill more data in the consolidation
    private ConsolidationDetails createConsolidationData() {
        int random = new Random().nextInt(100);
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().transportMode(TRANSPORT_MODES.get(random % TRANSPORT_MODES.size())).shipmentType(SHIPMENT_TYPE.get(random % SHIPMENT_TYPE.size()))
                .build();
        consolidationDetails.setTenantId(1);
        return consolidationDetails;
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

        ConsolidationDetailsRequest request = (ConsolidationDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is null for Consolidation Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        System.out.println(jsonHelper.convertToJson(request));
        ConsolidationDetails consolidationDetails = jsonHelper.convertValue(request, ConsolidationDetails.class);
        CarrierDetails carrierDetails = jsonHelper.convertValue(request.getCarrierDetails(), CarrierDetails.class);
        Allocations allocations = jsonHelper.convertValue(request.getAllocations(), Allocations.class);
        AchievedQuantities achievedQuantities = jsonHelper.convertValue(request.getAchievedQuantities(), AchievedQuantities.class);
        ArrivalDepartureDetails arrivalDepartureDetails = jsonHelper.convertValue(request.getArrivalDepartureDetails(), ArrivalDepartureDetails.class);

        try {

            if (carrierDetails != null) {
                createCarrier(carrierDetails);
                consolidationDetails.setCarrierDetails(carrierDetails);
            }

            if(allocations != null)
            {
                createAllocations(allocations);
                consolidationDetails.setAllocations(allocations);
            }

            if(achievedQuantities != null)
            {
                createAchievedQuantities(achievedQuantities);
                consolidationDetails.setAchievedQuantities(achievedQuantities);
            }

            if(arrivalDepartureDetails != null)
            {
                createArrivalDepartureDetails(arrivalDepartureDetails);
                consolidationDetails.setArrivalDepartureDetails(arrivalDepartureDetails);
            }

            if (request.getContainersList() != null) {
                List<ContainerRequest> containerRequest = request.getContainersList();
                List<Containers> containers = containerDao.updateEntityFromShipmentConsole(convertToEntityList(containerRequest, Containers.class));
                consolidationDetails.setContainersList(containers);
            }

            if(request.getShipmentsList() != null) {
                List<ShipmentRequest> shipmentRequest = request.getShipmentsList();
                List<ShipmentDetails> shipmentList = shipmentDao.saveShipments(convertToEntityList(shipmentRequest, ShipmentDetails.class));
                consolidationDetails.setShipmentsList(shipmentList);
            }

            getConsolidation(consolidationDetails);

            List<PackingRequest> packingRequest = request.getPackingList();
            if (packingRequest != null)
                createPackingsAsync(consolidationDetails, packingRequest);

            List<EventsRequest> eventsRequest = request.getEventsList();
            if (eventsRequest != null)
                createEventsAsync(consolidationDetails, eventsRequest);

            List<FileRepoRequest> fileRepoRequest = request.getFileRepoList();
            if (fileRepoRequest != null)
                createFileRepoAsync(consolidationDetails, fileRepoRequest);

            List<JobRequest> jobRequest = request.getJobsList();
            if (jobRequest != null)
                createJobsAsync(consolidationDetails, jobRequest);

            List<NotesRequest> notesRequest = request.getNotesList();
            if (notesRequest != null)
                createNotesAsync(consolidationDetails, notesRequest);

            List<ReferenceNumbersRequest> referenceNumbersRequest = request.getReferenceNumbersList();
            if (referenceNumbersRequest != null)
                createReferenceNumbersAsync(consolidationDetails, referenceNumbersRequest);

            List<RoutingsRequest> routingsRequest = request.getRoutingsList();
            if (routingsRequest != null)
                createRoutingsAsync(consolidationDetails, routingsRequest);

        } catch (Exception e) {
            log.error(e.getMessage());
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
        return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class));
    }

    void getConsolidation(ConsolidationDetails consolidationDetails) {
        consolidationDetails = consolidationDetailsDao.save(consolidationDetails);
    }


    private void createPartiesAsync(ConsolidationDetails consolidationDetails, List<PartiesRequest> partiesRequest) {
        partiesRequest.forEach(parties -> {
            createParties(consolidationDetails, parties);
        });
    }

    private void createRoutingsAsync(ConsolidationDetails consolidationDetails, List<RoutingsRequest> routingsRequest) {
        routingsRequest.forEach(routing -> {
            createRouting(consolidationDetails, routing);
        });
    }

    private void createReferenceNumbersAsync(ConsolidationDetails consolidationDetails, List<ReferenceNumbersRequest> referenceNumbersRequest) {
        referenceNumbersRequest.forEach(referenceNumber -> {
            createReferenceNumber(consolidationDetails, referenceNumber);
        });
    }

    private void createNotesAsync(ConsolidationDetails consolidationDetails, List<NotesRequest> notesRequest) {
        notesRequest.forEach(notes -> {
            createNote(consolidationDetails, notes);
        });
    }

    private void createJobsAsync(ConsolidationDetails consolidationDetails, List<JobRequest> jobRequest) {
        jobRequest.forEach(jobs -> {
            createJob(consolidationDetails, jobs);
        });
    }

    private void createFileRepoAsync(ConsolidationDetails consolidationDetails, List<FileRepoRequest> fileRepoRequest) {
        fileRepoRequest.forEach(fileRepo -> {
            createFileRepo(consolidationDetails, fileRepo);
        });
    }

    private void createEventsAsync(ConsolidationDetails consolidationDetails, List<EventsRequest> eventsRequest) {
        eventsRequest.forEach(event -> {
            createEvent(consolidationDetails, event);
        });
    }

    private void createPackingsAsync(ConsolidationDetails consolidationDetails, List<PackingRequest> packingRequest) {
        packingRequest.forEach(packing -> {
            createPacking(consolidationDetails, packing);
        });

    }

    private List<Containers> createContainersAsync(List<ContainerRequest> containerRequest) {
        return containerDao.saveAll(containerRequest.stream().map(e -> modelMapper.map(e, Containers.class)).collect(Collectors.toList()));
    }

    @Transactional
    public void createEvent(ConsolidationDetails consolidationDetails, EventsRequest eventsRequest) {
        eventsRequest.setConsolidationId(consolidationDetails.getId());
        eventDao.save(modelMapper.map(eventsRequest, Events.class));
    }

    @Transactional
    public void createFileRepo(ConsolidationDetails consolidationDetails, FileRepoRequest fileRepoRequest) {
        fileRepoRequest.setEntityId(consolidationDetails.getId());
        fileRepoRequest.setEntityType(Constants.CONSOLIDATION);
        fileRepoDao.save(modelMapper.map(fileRepoRequest, FileRepo.class));
    }

    @Transactional
    public void createJob(ConsolidationDetails consolidationDetails, JobRequest jobRequest) {
        jobRequest.setConsolidationId(consolidationDetails.getId());
        jobDao.save(modelMapper.map(jobRequest, Jobs.class));
    }

    @Transactional
    public void createNote(ConsolidationDetails consolidationDetails, NotesRequest notesRequest) {
        notesRequest.setEntityId(consolidationDetails.getId());
        notesRequest.setEntityType(Constants.CONSOLIDATION);
        notesDao.save(modelMapper.map(notesRequest, Notes.class));
    }

    @Transactional
    public void createParties(ConsolidationDetails consolidationDetails, PartiesRequest partiesRequest) {
        partiesRequest.setEntityId(consolidationDetails.getId());
        partiesRequest.setEntityType(Constants.CONSOLIDATION);
        packingDao.save(modelMapper.map(partiesRequest, Packing.class));
    }

    @Transactional
    public void createReferenceNumber(ConsolidationDetails consolidationDetails, ReferenceNumbersRequest referenceNumbersRequest) {
        referenceNumbersRequest.setConsolidationId(consolidationDetails.getId());
        referenceNumbersDao.save(modelMapper.map(referenceNumbersRequest, ReferenceNumbers.class));
    }

    @Transactional
    public void createPacking(ConsolidationDetails consolidationDetails, PackingRequest packingRequest) {
        packingRequest.setConsolidationId(consolidationDetails.getId());
        packingDao.save(modelMapper.map(packingRequest, Packing.class));
    }

    @Transactional
    public void createRouting(ConsolidationDetails consolidationDetails, RoutingsRequest routingsRequest) {
        routingsRequest.setConsolidationId(consolidationDetails.getId());
        routingsDao.save(modelMapper.map(routingsRequest, Routings.class));
    }

    @Transactional
    public void createCarrier(CarrierDetails carrierDetails) {
        carrierDao.save(carrierDetails);
    }

    @Transactional
    public void createAllocations(Allocations allocations) {
        allocationsDao.save(allocations);
    }

    @Transactional
    public void createAchievedQuantities(AchievedQuantities achievedQuantities) {
        achievedQuantitiesDao.save(achievedQuantities);
    }

    @Transactional
    public void createArrivalDepartureDetails(ArrivalDepartureDetails arrivalDepartureDetails) {
        arrivalDepartureDetailsDao.save(arrivalDepartureDetails);
    }

    @Override
    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ConsolidationDetailsRequest request = (ConsolidationDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Consolidation update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if (request.getId() == null) {
            log.error("Request Id is null for Consolidation update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        // TODO- implement Validation logic
        long id = request.getId();
        Optional<ConsolidationDetails> oldEntity = consolidationDetailsDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Consolidation Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ConsolidationDetails entity = modelMapper.map(request, ConsolidationDetails.class);
        entity.setId(oldEntity.get().getId());
        if (entity.getContainersList() == null)
            entity.setContainersList(oldEntity.get().getContainersList());
        if(entity.getShipmentsList() == null) {
            entity.setShipmentsList(oldEntity.get().getShipmentsList());
        }
        entity = consolidationDetailsDao.save(entity);
        return ResponseHelper.buildSuccessResponse(modelMapper.map(entity, ConsolidationDetailsResponse.class));
    }

    @Transactional
    public ResponseEntity<?> completeUpdate(CommonRequestModel commonRequestModel) throws Exception {

        ConsolidationDetailsRequest consolidationDetailsRequest = (ConsolidationDetailsRequest) commonRequestModel.getData();

        List<PackingRequest> packingRequestList = consolidationDetailsRequest.getPackingList();
        List<ContainerRequest> containerRequestList = consolidationDetailsRequest.getContainersList();
        List<EventsRequest> eventsRequestList = consolidationDetailsRequest.getEventsList();
        List<FileRepoRequest> fileRepoRequestList = consolidationDetailsRequest.getFileRepoList();
        List<JobRequest> jobRequestList = consolidationDetailsRequest.getJobsList();
        List<NotesRequest> notesRequestList = consolidationDetailsRequest.getNotesList();
        List<ReferenceNumbersRequest> referenceNumbersRequestList = consolidationDetailsRequest.getReferenceNumbersList();
        List<RoutingsRequest> routingsRequestList = consolidationDetailsRequest.getRoutingsList();
        CarrierDetailRequest carrierDetailRequest = consolidationDetailsRequest.getCarrierDetails();
        AllocationsRequest allocationsRequest = consolidationDetailsRequest.getAllocations();
        AchievedQuantitiesRequest achievedQuantitiesRequest = consolidationDetailsRequest.getAchievedQuantities();
        ArrivalDepartureDetailsRequest arrivalDepartureDetailsRequest = consolidationDetailsRequest.getArrivalDepartureDetails();

        // TODO- implement Validation logic
        long id = consolidationDetailsRequest.getId();
        Optional<ConsolidationDetails> oldEntity = consolidationDetailsDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Consolidation Details is null for Id {}", consolidationDetailsRequest.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try {

            ConsolidationDetails entity = modelMapper.map(consolidationDetailsRequest, ConsolidationDetails.class);
            entity.setId(oldEntity.get().getId());
            List<Containers> updatedContainers = null;
            if (containerRequestList != null) {
                updatedContainers = containerDao.updateEntityFromShipmentConsole(convertToEntityList(containerRequestList, Containers.class));
            } else {
                updatedContainers = oldEntity.get().getContainersList();
            }
            entity.setContainersList(updatedContainers);
            CarrierDetails updatedCarrierDetails = null;
            if (carrierDetailRequest != null) {
                updatedCarrierDetails = carrierDao.updateEntityFromShipmentConsole(convertToClass(carrierDetailRequest, CarrierDetails.class));
                entity.setCarrierDetails(updatedCarrierDetails);
            }
            Allocations updatedAllocations = null;
            if(allocationsRequest != null)
            {
                updatedAllocations = allocationsDao.updateEntityFromShipmentConsole(convertToClass(allocationsRequest, Allocations.class));
                entity.setAllocations(updatedAllocations);
            }
            ArrivalDepartureDetails updatedArrivalDepartureDetails = null;
            if(arrivalDepartureDetailsRequest != null)
            {
                updatedArrivalDepartureDetails = arrivalDepartureDetailsDao.updateEntityFromShipmentConsole(convertToClass(arrivalDepartureDetailsRequest, ArrivalDepartureDetails.class));
                entity.setArrivalDepartureDetails(updatedArrivalDepartureDetails);
            }
            AchievedQuantities updatedAchievedQuantities = null;
            if(achievedQuantitiesRequest != null)
            {
                updatedAchievedQuantities = achievedQuantitiesDao.updateEntityFromShipmentConsole(convertToClass(achievedQuantitiesRequest, AchievedQuantities.class));
                entity.setAchievedQuantities(updatedAchievedQuantities);
            }
            entity = consolidationDetailsDao.save(entity);

            ConsolidationDetailsResponse response = modelMapper.map(entity, ConsolidationDetailsResponse.class);
            response.setContainersList(updatedContainers.stream().map(e -> modelMapper.map(e, ContainerResponse.class)).collect(Collectors.toList()));
            if (carrierDetailRequest != null) {
                response.setCarrierDetails(convertToClass(updatedCarrierDetails, CarrierDetailResponse.class));
            }
            if (packingRequestList != null) {
                List<Packing> updatedPackings = packingDao.updateEntityFromShipment(convertToEntityList(packingRequestList, Packing.class), id);
                response.setPackingList(convertToDtoList(updatedPackings, PackingResponse.class));
            }
            if (eventsRequestList != null) {
                List<Events> updatedEvents = eventDao.updateEntityFromShipment(convertToEntityList(eventsRequestList, Events.class), id);
                response.setEventsList(convertToDtoList(updatedEvents, EventsResponse.class));
            }
            if (fileRepoRequestList != null) {
                List<FileRepo> updatedFileRepos = fileRepoDao.updateEntityFromShipment(convertToEntityList(fileRepoRequestList, FileRepo.class), id);
                response.setFileRepoList(convertToDtoList(updatedFileRepos, FileRepoResponse.class));
            }
            if (jobRequestList != null) {
                List<Jobs> updatedJobs = jobDao.updateEntityFromShipment(convertToEntityList(jobRequestList, Jobs.class), id);
                response.setJobsList(convertToDtoList(updatedJobs, JobResponse.class));
            }
            if (notesRequestList != null) {
                List<Notes> updatedNotes = notesDao.updateEntityFromShipment(convertToEntityList(notesRequestList, Notes.class), id);
                response.setNotesList(convertToDtoList(updatedNotes, NotesResponse.class));
            }
            if (referenceNumbersRequestList != null) {
                List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.updateEntityFromShipment(convertToEntityList(referenceNumbersRequestList, ReferenceNumbers.class), id);
                response.setReferenceNumbersList(convertToDtoList(updatedReferenceNumbers, ReferenceNumbersResponse.class));
            }
            if (routingsRequestList != null) {
                List<Routings> updatedRoutings = routingsDao.updateEntityFromShipment(convertToEntityList(routingsRequestList, Routings.class), id);
                response.setRoutingsList(convertToDtoList(updatedRoutings, RoutingsResponse.class));
            }

            return ResponseHelper.buildSuccessResponse(response);
        } catch (ExecutionException e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> completeRetrieveById(CommonRequestModel commonRequestModel) throws ExecutionException, InterruptedException {
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

            CompletableFuture<ResponseEntity<?>> consolidationsFuture = retrieveByIdAsync(commonRequestModel);
            RunnerResponse<ConsolidationDetailsResponse> res = (RunnerResponse<ConsolidationDetailsResponse>) consolidationsFuture.get().getBody();

            return ResponseHelper.buildSuccessResponse(res.getData());
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            // TODO- implement actual logic with filters
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Consolidation list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<ConsolidationDetails>, Pageable> tuple = fetchData(request, ConsolidationDetails.class, tableNames);
            Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Consolidation list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(consolidationDetailsPage.getContent()),
                    consolidationDetailsPage.getTotalPages(),
                    consolidationDetailsPage.getTotalElements());
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

    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
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
                log.debug("Consolidation Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            consolidationDetailsDao.delete(consolidationDetails.get());
            log.info("Deleted Consolidation details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
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
                log.error("Request is empty for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Consolidation retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(id);
            if (!consolidationDetails.isPresent()) {
                log.debug("Consolidation Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Consolidation details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ConsolidationDetailsResponse response = jsonHelper.convertValue(consolidationDetails.get(), ConsolidationDetailsResponse.class);
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
                log.error("Request is empty for Consolidation async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Consolidation async retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(id);
            if (!consolidationDetails.isPresent()) {
                log.debug("Consolidation Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Consolidation details async fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ConsolidationDetailsResponse response = jsonHelper.convertValue(consolidationDetails.get(), ConsolidationDetailsResponse.class);
            return CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(response));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
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

    private <T extends IRunnerResponse> T getResponseEntity(ResponseEntity<?> responseEntity) throws ExecutionException, InterruptedException {
        var runnerResponse = (RunnerResponse<T>) responseEntity.getBody();
        return (T) runnerResponse.getData();
    }
}
