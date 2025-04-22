package com.dpw.runner.shipment.services.adapters.impl;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;

import com.azure.messaging.servicebus.ServiceBusMessage;
import com.dpw.runner.shipment.services.adapters.config.TrackingServiceConfig;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.request.TrackingRequest;
import com.dpw.runner.shipment.services.dto.response.TrackingEventsResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiRequest;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.DateAndSources;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Details;
import com.dpw.runner.shipment.services.dto.trackingservice.TrackingServiceApiResponse.Event;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.CountryListHelper;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.nimbusds.jose.util.Pair;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ForkJoinPool;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;


@Slf4j
@Service
public class TrackingServiceAdapter implements ITrackingServiceAdapter {

    @Autowired
    private IAwbDao awbDao;
    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private ISBProperties isbProperties;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private MasterDataFactory masterDataFactory;
    @Autowired
    @Qualifier("restTemplateForTrackingService")
    private RestTemplate restTemplate;
    @Autowired
    private ISBUtils sbUtils;
    @Autowired
    private IShipmentDao shipmentDao;
    @Value("${trackingService.apiKey}")
    private String trackingServiceApiKey;
    @Autowired
    private TrackingServiceConfig trackingServiceConfig;
    @Value("${trackingService.newFlowEndpoint.url}")
    private String trackingServiceNewFlowEndpoint;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private CommonUtils commonUtils;

    @Override
    public UniversalTrackingPayload.UniversalEventsPayload mapEventDetailsForTracking(String bookingReferenceNumber, String referenceNumberType, String runnerReferenceNumber, List<Events> events) {
        return UniversalTrackingPayload.UniversalEventsPayload.builder()
            .runnerReferenceNumber(runnerReferenceNumber)
            .referenceNumberType(referenceNumberType)
            .bookingReferenceNumber(bookingReferenceNumber)
            .events(mapEvents(events))
            .build();
    }


    public List<UniversalTrackingPayload.EventDetail> mapEvents(List<Events> events)
    {
        List<UniversalTrackingPayload.EventDetail> infoList = new ArrayList<>();
        events.forEach(i -> {
            UniversalTrackingPayload.EventDetail eventDetail = UniversalTrackingPayload.EventDetail.builder()
                    .id(i.getId())
                    .description(i.getDescription())
                    .eventCode(i.getEventCode())
                    .estimated(i.getEstimated())
                    .actual(i.getActual())
                    .placeName(i.getPlaceName())
                    .build();
            infoList.add(eventDetail);
        });
        return infoList;
    }

    @Override
    @Async
    public void publishUpdatesToTrackingServiceQueue(String jsonBody, Boolean isTrackingEvents) {
        if(Boolean.TRUE.equals(isTrackingEvents)){
          // Publish to EventsMessage Topic
          sbUtils.sendMessagesToTopic(
              isbProperties,
              trackingServiceConfig.getEventsMessageTopic(),
              List.of(new ServiceBusMessage(jsonBody)));
        } else {
            sbUtils.sendMessagesToTopic(
                    isbProperties,
                    trackingServiceConfig.getRunnerFlowMessageTopic(),
                    List.of(new ServiceBusMessage(jsonBody)));
        }
    }

    @Override
    public boolean checkIfConsolAttached(ShipmentDetails shipmentDetails) {
        boolean res = false;
        try {
            List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentId(shipmentDetails.getId());
            ConsolidationDetails consolidationDetails = null;
            if(consoleShipmentMappings != null && !consoleShipmentMappings.isEmpty()) {
                Optional<ConsolidationDetails> optional = consolidationDetailsDao.findById(consoleShipmentMappings.get(0).getConsolidationId());
                consolidationDetails = optional.get();
            }

            if(consolidationDetails != null && !consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
                res = !getContainerDetailsAttachedForShipment(shipmentDetails).isEmpty();
            else
                res = !Objects.isNull(shipmentDetails.getHouseBill()) || !Objects.isNull(shipmentDetails.getMasterBill());

        } catch (Exception e) {
            log.error(e.getMessage());
        }
        return res;
    }

    @Override
    public boolean checkIfAwbExists(ConsolidationDetails consolidationDetails) {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByConsolidationId(consolidationDetails.getId());
        if(consoleShipmentMappings != null && !consoleShipmentMappings.isEmpty()){
            Optional<ShipmentDetails> optional = shipmentDao.findById(consoleShipmentMappings.get(0).getShipmentId());
            if (optional.isPresent()) {
                var shipment = optional.get();
                return (shipment.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipment.getHouseBill() != null);
            }
        }
        return false;
    }

    @Override
    public boolean checkIfConsolContainersExist(ConsolidationDetails consolidationDetails) {
        boolean res = false;
        if(consolidationDetails.getContainersList() != null) {
            res = true;
            for(var c : consolidationDetails.getContainersList()) {
                res = res && (!Objects.isNull(c.getContainerNumber()));
            }
        }
        return res;
    }

    @Override
    public UniversalTrackingPayload mapConsoleDataToTrackingServiceData(ConsolidationDetails consolidationDetails, ShipmentDetails shipment) {
        UniversalTrackingPayload trackingPayload = null;
        if(consolidationDetails != null) {
            trackingPayload = mapDetailsToTSData(consolidationDetails, shipment, true);
            log.info("Consolidation tracking payload : {}", trackingPayload);
        }
        return trackingPayload;
    }

    @Override
    public UniversalTrackingPayload mapShipmentDataToTrackingServiceData(ShipmentDetails shipmentDetails) {
        UniversalTrackingPayload trackingPayload = null;
        ConsolidationDetails consol = getConsolidationFromShipment(shipmentDetails.getId());
        trackingPayload = mapDetailsToTSData(consol, shipmentDetails, true);
        log.info("Shipment Tracking Update: {}", trackingPayload.toString());
        return trackingPayload;
    }

    @Override
    public List<Events> getAllEvents(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails, String refNumber) {
        // Modified Logic (currently returning only shipment/ consol events)
        // This will be replaced once the below to-do item is completed
        List<Events> allEvents = new ArrayList<>();
        if (shipmentDetails != null) {
            allEvents.addAll(shipmentDetails.getEventsList() != null ? shipmentDetails.getEventsList() : Collections.emptyList());
            // Fetch Consol events based on ref number
            if(StringUtility.isNotEmpty(refNumber)) {
                allEvents.addAll(getEventsFromConsolidation(refNumber));
            }
        }
        if(consolidationDetails != null) {
            allEvents.addAll(consolidationDetails.getEventsList() != null ? consolidationDetails.getEventsList() : Collections.emptyList());
            // Fetch shipment events based on ref number
            allEvents.addAll(getEventsFromShipment(refNumber));
        }

        allEvents = allEvents.stream().filter(i -> (i.getIsPublicTrackingEvent() == null || !i.getIsPublicTrackingEvent())).toList();
        return allEvents;
    }

    private List<Events> getEventsFromShipment(String refNumber) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("bookingReference", refNumber, "=");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
        Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(pair.getLeft(), pair.getRight());
        if(!shipmentDetailsPage.isEmpty()) {
            return shipmentDetailsPage.getContent().get(0).getEventsList();
        }
        return Collections.emptyList();
    }

    private List<Events> getEventsFromConsolidation(String refNumber) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("referenceNumber", refNumber, "=");
        Pair<Specification<ConsolidationDetails>, Pageable> pair = fetchData(listCommonRequest, ConsolidationDetails.class);
        Page<ConsolidationDetails> consolidationDetailsPage = consolidationDetailsDao.findAll(pair.getLeft(), pair.getRight());
        if(!consolidationDetailsPage.isEmpty()) {
            return consolidationDetailsPage.getContent().get(0).getEventsList();
        }
        return Collections.emptyList();
    }

    private ConsolidationDetails getConsolidationFromShipment(Long shipmentId) {
        ConsolidationDetails consolidationDetails = null;
        try{
            List<ConsoleShipmentMapping> linkedConsol = consoleShipmentMappingDao.findByShipmentId(shipmentId);
            if(linkedConsol != null && !linkedConsol.isEmpty()) {
                Optional<ConsolidationDetails> optional = consolidationDetailsDao.findById(linkedConsol.get(0).getConsolidationId());
                consolidationDetails = optional.get();
            }
        } catch (Exception e) {
            log.error("Error while fetching linked consol from shipment with id {}", shipmentId);
        }
        return  consolidationDetails;
    }

    private UniversalTrackingPayload mapDetailsToTSData(ConsolidationDetails inputConsol, ShipmentDetails inputShipment, boolean isRequestFromShipment) {
        UniversalTrackingPayload trackingPayload = null;
        List<UniversalTrackingPayload.ShipmentDetail> shipmentDetails = new ArrayList<>();
        var shipDetails = getShipmentDetails(inputShipment);

        if(shipDetails != null)
            shipmentDetails.add(shipDetails);

        var entityDetails = getEntityDetails(inputConsol, inputShipment, isRequestFromShipment);
        var consolNumber = inputConsol !=null ? inputConsol.getConsolidationNumber() : null;
        var masterBill = inputConsol !=null ? inputConsol.getBol() : getDefaultMasterBill(inputShipment);
        var shipmentNumber =  inputShipment !=null ? inputShipment.getShipmentId() : null;


        if(!isRequestFromShipment)
            trackingPayload = mapDetailsForTracking(Constants.CONSOLIDATION, consolNumber,masterBill, shipmentDetails, entityDetails);
        else
            trackingPayload = mapDetailsForTracking(Constants.SHIPMENT, shipmentNumber,masterBill, shipmentDetails, entityDetails);

        setBookingReferenceNumberInTrackingPayload(inputConsol, inputShipment, isRequestFromShipment, trackingPayload);

        setEntityTypeInTrackingPayload(inputConsol, inputShipment, trackingPayload);

        setCarrierInTrackingPayload(inputConsol, inputShipment, trackingPayload);

        return trackingPayload;
    }

    private String getDefaultMasterBill(ShipmentDetails inputShipment) {
        return inputShipment != null ? inputShipment.getMasterBill() : null;
    }

     public void setBookingReferenceNumberInTrackingPayload(ConsolidationDetails inputConsol, ShipmentDetails inputShipment, boolean isRequestFromShipment, UniversalTrackingPayload trackingPayload) {
        if(inputShipment != null && "API".equals(inputShipment.getSource())) {
            if(!isRequestFromShipment) {
                var referenceNumber = inputConsol !=  null ? inputConsol.getReferenceNumber() : null;
                trackingPayload.setBookingReferenceNumber(referenceNumber);
            }
            else
                trackingPayload.setBookingReferenceNumber(inputShipment.getBookingReference());
        }
        else
            trackingPayload.setBookingReferenceNumber(null);
    }

    private void setEntityTypeInTrackingPayload(ConsolidationDetails inputConsol, ShipmentDetails inputShipment, UniversalTrackingPayload trackingPayload) {
        if((inputConsol != null && ! inputConsol.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) || (inputShipment != null && !inputShipment.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)))
            trackingPayload.setEntityType("Container");
        else
            trackingPayload.setEntityType("awb");
    }

    private void setCarrierInTrackingPayload(ConsolidationDetails inputConsol, ShipmentDetails inputShipment, UniversalTrackingPayload trackingPayload) {
        if(inputConsol != null)
            trackingPayload.setCarrier(fetchCarrierName(inputConsol.getCarrierDetails().getShippingLine()));
        else
            trackingPayload.setCarrier(fetchCarrierName(inputShipment.getCarrierDetails().getShippingLine()));
    }

    private List<UniversalTrackingPayload.EntityDetail> getEntityDetails(ConsolidationDetails inputConsol, ShipmentDetails inputShipment, boolean isRequestFromShipment) {
        if(inputConsol!=null && !inputConsol.getTransportMode().equals("AIR")) {
            if(!isRequestFromShipment)
                return getContainerDetailsFromConsol(inputConsol);
            else
                return getContainerDetailsAttachedForShipment(inputShipment);
        }
        else if(inputShipment.getTransportMode().equals("AIR"))
                return getAWBDetailsFromShipment(inputShipment);
        else
            return null;
    }

    private String getBillOfLading(ShipmentDetails shipmentDetails) {
        if(!isStringNullOrEmpty(shipmentDetails.getHouseBill()))
            return shipmentDetails.getHouseBill();
        return shipmentDetails.getMasterBill();
    }

    private List<UniversalTrackingPayload.EntityDetail> getAWBDetailsFromShipment(ShipmentDetails inputShipment) {
        List<UniversalTrackingPayload.EntityDetail> result = new ArrayList<>();
        List<Awb> awbList = awbDao.findByShipmentId(inputShipment.getId());
        result.add(UniversalTrackingPayload.EntityDetail.builder()
                .trackingNumber(inputShipment.getMasterBill())
                .allocationDate(awbList == null || awbList.isEmpty() ? null: awbList.get(0).getCreatedAt().format(DateTimeFormatter.ofPattern("yyyy-MM-dd")))
                .build());

        return result;
    }

    private List<UniversalTrackingPayload.EntityDetail> getContainerDetailsAttachedForShipment(ShipmentDetails inputShipment) {
        return getEntityDetailsFromContainers(inputShipment.getContainersList() != null ? new ArrayList<>(inputShipment.getContainersList()) : null);
    }


    private List<UniversalTrackingPayload.EntityDetail> getContainerDetailsFromConsol(ConsolidationDetails inputConsol) {
        return getEntityDetailsFromContainers(inputConsol.getContainersList());
    }

    private List<UniversalTrackingPayload.EntityDetail> getEntityDetailsFromContainers(List<Containers> containersList) {
        if(containersList == null || containersList.isEmpty())
            return null;

        List<UniversalTrackingPayload.EntityDetail> result = new ArrayList<>();
        for (var container : containersList) {
            var entityDetail = UniversalTrackingPayload.EntityDetail.builder()
                    .trackingNumber(container.getContainerNumber())
                    .allocationDate(container.getAllocationDate() != null ? (DateTimeFormatter.ofPattern("yyyy-MM-dd").format(container.getAllocationDate())) : null)
                    .grossWeight(container.getGrossWeight())
                    .grossWeightUom(container.getGrossWeightUnit())
                    .isEmpty(container.getIsEmpty())
                    .isReefer(container.getIsReefer())
                    .isShipperOwned(container.getIsShipperOwned())
                    .containerTypeCode(container.getContainerCode())
                    .mode(container.getHblDeliveryMode())
                    .containerCount(container.getContainerCount())
                    .descriptionOfGoods(container.getDescriptionOfGoods())
                    .noofPackages(isStringNullOrEmpty(container.getPacks()) ? null : Long.valueOf(container.getPacks()))
                    .netWeight(container.getNetWeight())
                    .netWeightUom(container.getNetWeightUnit())
                    .build();
            result.add(entityDetail);
        }
        return result;
    }

    private String fetchCarrierName(String carrier) {
        if(StringUtility.isEmpty(carrier)) return null;
        List<Object> carrierCriteria = Arrays.asList(
                List.of("ItemValue"),
                "=",
                carrier
        );
        CommonV1ListRequest carrierRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(carrierCriteria).build();
        CarrierListObject carrierListObject = new CarrierListObject();
        carrierListObject.setListObject(carrierRequest);
        Object carrierResponse = masterDataFactory.getMasterDataService().fetchCarrierMasterData(carrierListObject).getData();
        List<CarrierMasterData> carrierMasterData = jsonHelper.convertValueToList(carrierResponse, CarrierMasterData.class);
        if(carrierMasterData == null || carrierMasterData.isEmpty())
            return null;
        return carrierMasterData.get(0).getIdentifier1();
    }

    private UniversalTrackingPayload.ShipmentDetail getShipmentDetails(ShipmentDetails shipmentDetails) {
        if(shipmentDetails == null)
            return null;

        // shipment direction
        String shipmentDirection = shipmentDetails.getDirection();
        if(Constants.DIRECTION_CTS.equalsIgnoreCase(shipmentDetails.getDirection()))
            shipmentDirection = Constants.DIRECTION_EXP;


        UniversalTrackingPayload.ShipmentDetail response = UniversalTrackingPayload.ShipmentDetail.builder()
                .serviceMode(shipmentDetails.getServiceType())
//                .estimatedPickupDate(shipmentDetails.EstimatedPickup != null ? ((DateTime)shipmentDetails.EstimatedPickup).Date.ToString("yyyy-MM-dd") : null)
//                .bookingCreationDate(shipmentDetails.DateofIssue != null ? ((DateTime)shipmentDetails.DateofIssue).Date.ToString("yyyy-MM-dd") : null)
                .houseBill(getBillOfLading(shipmentDetails))
                .shipmentType(shipmentDirection)
                .countryCode(getBranchCountryCode())
                .build();

        if(shipmentDetails.getCarrierDetails() != null) {
            CarrierDetails cd = shipmentDetails.getCarrierDetails();
            Set<String> locationRefGuids = new HashSet<>();
            locationRefGuids.add(cd.getOrigin());
            locationRefGuids.add(cd.getDestination());
            locationRefGuids.add(cd.getOriginPort());
            locationRefGuids.add(cd.getDestinationPort());
            Map<String, UnlocationsResponse> unlocationsResponseMap = getLocationData(locationRefGuids);
            response.setOriginName(unlocationsResponseMap.get(cd.getOrigin()) != null ? unlocationsResponseMap.get(cd.getOrigin()).getName() : null);
            response.setOriginPortCode(unlocationsResponseMap.get(cd.getOriginPort()) != null ? unlocationsResponseMap.get(cd.getOriginPort()).getLocCode() : null);
            response.setDestinationName(unlocationsResponseMap.get(cd.getDestination()) != null ? unlocationsResponseMap.get(cd.getDestination()).getName() : null);
            response.setDestinationCountry(unlocationsResponseMap.get(cd.getDestination()) != null ? unlocationsResponseMap.get(cd.getDestination()).getCountry() : null);
            response.setDestinationPortCode(unlocationsResponseMap.get(cd.getDestinationPort()) != null ? unlocationsResponseMap.get(cd.getDestinationPort()).getLocCode() : null);
        }
        return response;
    }

    private UniversalTrackingPayload mapDetailsForTracking(String referenceNumberType, String runnerReferenceNumber, String masterBill, List<UniversalTrackingPayload.ShipmentDetail> shipmentDetail, List<UniversalTrackingPayload.EntityDetail> entityDetail) {
        return UniversalTrackingPayload.builder()
        .runnerReferenceNumber(runnerReferenceNumber)
        .referenceNumberType(referenceNumberType)
        .shipmentDetails(shipmentDetail)
        .entityDetails(entityDetail)
        .masterBill(masterBill)
        .build();
    }

    private Map<String, UnlocationsResponse> getLocationData(Set<String> locCodes) {
        Map<String, UnlocationsResponse> locationMap = new HashMap<>();
        if (!locCodes.isEmpty()) {
            List<Object> criteria = Arrays.asList(
                    Arrays.asList(EntityTransferConstants.LOCATION_SERVICE_GUID),
                    "In",
                    Arrays.asList(locCodes)
            );
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
            V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
            List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
            if (unlocationsResponse != null && !unlocationsResponse.isEmpty()) {

                for (UnlocationsResponse unlocation : unlocationsResponse) {
                    locationMap.put(unlocation.getLocationsReferenceGUID(), unlocation);
                }

            }
        }
        return locationMap;
    }

    private String getBranchCountryCode() {
        String countryCode = null;
        // user should ideally not be null but even if it is we can gracefully exit
        if (UserContext.getUser() == null || UserContext.getUser().getTenantCountryCode() == null)
            return countryCode;

        countryCode = CountryListHelper.ISO3166.fromAlpha3(UserContext.getUser().getTenantCountryCode().toUpperCase()).getAlpha2();

        return StringUtility.toUpperCase(countryCode);
    }

    @Override
    public TrackingServiceApiResponse fetchTrackingData(TrackingRequest request) throws RunnerException {
        var headers = new HttpHeaders();
        headers.add(ApiConstants.X_API_KEY, trackingServiceApiKey);
        headers.setContentType(MediaType.APPLICATION_JSON);
        var httpEntity = new HttpEntity<>(List.of(TrackingServiceApiRequest.builder().shipmentReference(request.getReferenceNumber()).build()), headers);

        try {
            log.info("Entered tracking call");
            var response = restTemplate.postForEntity(trackingServiceNewFlowEndpoint, httpEntity, TrackingServiceApiResponse.class);
            var responseBody = response.getBody();
            log.info("Received response from tracking {}", responseBody);
            return responseBody;
        } catch (Exception e){
            log.error("Error while calling tracking endpoint ", e);
            throw new RunnerException(e.getMessage());
        }
    }

    // WILL REMOVE AFTER EVENTS TESTING
    // IGNORE THE COMMENTED SECTION

    @SuppressWarnings("java:S125")
//    @Override
//    public TrackingServiceApiResponse fetchTrackingData(TrackingRequest request) throws RunnerException {
//        // Toggle this flag to switch between remote call and reading from file
//        boolean useLocalJson = true; // Set this to true to use JSON file A
//
//        if (useLocalJson) {
//            return fetchFromJsonFile("src/main/resources/ts_payload_sample.json");
//        }
//
//        // Existing code for remote call
//        var headers = new HttpHeaders();
//        headers.add(ApiConstants.X_API_KEY, trackingServiceApiKey);
//        headers.setContentType(MediaType.APPLICATION_JSON);
//        var httpEntity = new HttpEntity<>(List.of(
//                TrackingServiceApiRequest.builder()
//                        .shipmentReference(request.getReferenceNumber())
//                        .build()), headers);
//
//        try {
//            log.info("Entered tracking call");
//            var response = restTemplate.postForEntity(
//                    trackingServiceNewFlowEndpoint,
//                    httpEntity,
//                    TrackingServiceApiResponse.class);
//            var responseBody = response.getBody();
//            log.info("Received response from tracking {}", responseBody);
//            return responseBody;
//        } catch (Exception e) {
//            log.error("Error while calling tracking endpoint ", e);
//            throw new RunnerException(e.getMessage());
//        }
//    }
//
//    // Method to read the JSON file and convert it into TrackingServiceApiResponse
//    private TrackingServiceApiResponse fetchFromJsonFile(String filePath) throws RunnerException {
//        try {
//            ObjectMapper objectMapper = new ObjectMapper();
//            objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
//            objectMapper.registerModule(new JavaTimeModule());
//            // Assuming the JSON file is on the classpath, adjust if needed
//
//            return objectMapper.readValue(new File(filePath), TrackingServiceApiResponse.class);
//        } catch (IOException e) {
//            log.error("Error reading JSON file", e);
//            throw new RunnerException("Error reading JSON file: " + e.getMessage());
//        }
//    }

    @Override
    public String convertTrackingEventCodeToShortCode(Event event) {

        String safeEventType = StringUtils.defaultString(event.getEventType());
        String safeLocationRole = StringUtils.defaultString(event.getLocationRole());
        String safeDescription = StringUtils.defaultString(event.getDescription());
        String safeDescriptionFromSource = StringUtils.defaultString(event.getDescriptionFromSource());

        log.info("Converting event code '{}' with location role '{}'", safeEventType, safeLocationRole);

        if (EventConstants.FLIGHT_ARRIVAL.equalsIgnoreCase(safeEventType)
                && safeDescription.equalsIgnoreCase("Flight Arrival")) {
            log.info("Matched FLIGHT_ARRIVAL and DESCRIPTION. Returning short code: {}", EventConstants.FLAR);
            return EventConstants.FLAR;
        }

        if (EventConstants.FLIGHT_DEPARTURE.equalsIgnoreCase(safeEventType)
                && safeDescription.equalsIgnoreCase("Flight Departure")) {
            log.info("Matched FLIGHT_DEPARTURE and DESCRIPTION. Returning short code: {}", EventConstants.FLDR);
            return EventConstants.FLDR;
        }

        String shortCode = getShortCode(safeEventType, safeDescription);
        if (shortCode != null) {
            return shortCode;
        }

        String ecpk = getECPKEventCode(safeEventType, safeLocationRole);
        if (ecpk != null) {
            return ecpk;
        }

        String fcgi = getFCGIEventCode(safeEventType, safeLocationRole);
        if (fcgi != null) {
            return fcgi;
        }

        String vsdp = getVSDPEventCode(safeEventType, safeLocationRole, safeDescriptionFromSource);
        if (vsdp != null) {
            return vsdp;
        }

        String ardp = getADRPEventCode(safeEventType, safeLocationRole);
        if (ardp != null) {
            return ardp;
        }

        String fugo = getFUGOEventCode(safeEventType, safeLocationRole);
        if (fugo != null) {
            return fugo;
        }

        String emcr = getEMCREventCode(safeEventType, safeLocationRole);
        if (emcr != null) {
            return emcr;
        }

        log.info("No match found for event code '{}' with location role '{}'. Returning original event code.", safeEventType, safeLocationRole);
        return safeEventType;
    }

    private String getShortCode(String safeEventCode, String safeDescription) {
        if (EventConstants.LITERAL.equalsIgnoreCase(safeEventCode)) {
            String shortCode = null;

            if (safeDescription.equalsIgnoreCase("Received from Flight")) {
                shortCode = EventConstants.TRCF;
            } else if (safeDescription.equalsIgnoreCase("Consignee notified")) {
                shortCode = EventConstants.TNFD;
            } else if (safeDescription.equalsIgnoreCase("Received from Shipper")) {
                shortCode = EventConstants.TRCS;
            }

            if (shortCode != null) {
                log.info("Matched LITERAL and DESCRIPTION. Returning short code: {}", shortCode);
                return shortCode;
            }
        }
        return null;
    }

    private String getECPKEventCode(String safeEventCode, String safeLocationRole) {
        if (EventConstants.GATE_IN_WITH_CONTAINER_EMPTY.equalsIgnoreCase(safeEventCode)
                && safeLocationRole.startsWith(EventConstants.ORIGIN)) {
            log.info("Matched GATE_IN_WITH_CONTAINER_EMPTY and ORIGIN. Returning short code: {}", EventConstants.ECPK);
            return EventConstants.ECPK;
        }
        return null;
    }

    private String getFCGIEventCode(String safeEventCode, String safeLocationRole) {
        if (EventConstants.GATE_IN_WITH_CONTAINER_FULL.equalsIgnoreCase(safeEventCode)
                && "originPort".equalsIgnoreCase(safeLocationRole)) {
            log.info("Matched GATE_IN_WITH_CONTAINER_FULL and originPort. Returning short code: {}", EventConstants.FCGI);
            return EventConstants.FCGI;
        }
        return null;
    }

    private String getVSDPEventCode(String safeEventType, String safeLocationRole, String safeDescriptionFromSource) {
        boolean isVesselDepartureFromOrigin =
                EventConstants.VESSEL_DEPARTURE_WITH_CONTAINER.equalsIgnoreCase(safeEventType) &&
                        EventConstants.ORIGIN_PORT.equalsIgnoreCase(safeLocationRole);

        boolean isExportLoadOnVessel =
                EventConstants.LOAD_ON_VESSEL.equalsIgnoreCase(safeEventType) &&
                        EventConstants.EXPORT_LOADED_ON_VESSEL.equalsIgnoreCase(safeDescriptionFromSource);

        if (isVesselDepartureFromOrigin || isExportLoadOnVessel) {
            log.info("Matched conditions for VSDP. Returning short code: {}", EventConstants.VSDP);
            return EventConstants.VSDP;
        }

        return null;
    }

    private String getADRPEventCode(String safeEventCode, String safeLocationRole) {
        if (EventConstants.VESSEL_ARRIVAL_WITH_CONTAINER.equalsIgnoreCase(safeEventCode)
                && "destinationPort".equalsIgnoreCase(safeLocationRole)) {
            log.info("Matched VESSEL_ARRIVAL_WITH_CONTAINER and destinationPort. Returning short code: {}", EventConstants.ARDP);
            return EventConstants.ARDP;
        }
        return null;
    }

    private String getFUGOEventCode(String safeEventCode, String safeLocationRole) {
        if (EventConstants.GATE_OUT_WITH_CONTAINER_FULL.equalsIgnoreCase(safeEventCode)
                && "destinationPort".equalsIgnoreCase(safeLocationRole)) {
            log.info("Matched GATE_OUT_WITH_CONTAINER_FULL and destinationPort. Returning short code: {}", EventConstants.FUGO);
            return EventConstants.FUGO;
        }
        return null;
    }

    private String getEMCREventCode(String safeEventCode, String safeLocationRole) {
        if (EventConstants.GATE_IN_WITH_CONTAINER_EMPTY.equalsIgnoreCase(safeEventCode)
                && safeLocationRole.startsWith(EventConstants.DESTINATION)) {
            log.info("Matched GATE_IN_WITH_CONTAINER_EMPTY and DESTINATION. Returning short code: {}", EventConstants.EMCR);
            return EventConstants.EMCR;
        }
        return null;
    }

    @Override
    public TrackingEventsResponse getTrackingEventsResponse(String referenceNumber) throws RunnerException {
        try {
            TrackingEventsResponse trackingEventsResponse = new TrackingEventsResponse();
            var trackingServiceApiResponse = fetchTrackingData(TrackingRequest.builder().referenceNumber(referenceNumber).build());
            trackingEventsResponse.setEventsList(generateEventsFromTrackingResponse(trackingServiceApiResponse));
            // Set ATA and ATD date based on container journey details
            var container = Optional.ofNullable(trackingServiceApiResponse.getContainers()) // Handle potential null list
                    .orElse(Collections.emptyList()).stream().filter(Objects::nonNull)
                    .findFirst()
                    .orElse(null);

            if (container != null && container.getJourney() != null) {
                trackingEventsResponse.setShipmentAta(Optional.ofNullable(container.getJourney().getPortOfArrivalAta())
                        .map(TrackingServiceApiResponse.DateAndSources::getDateTime).orElse(null));
                trackingEventsResponse.setShipmentAtd(Optional.ofNullable(container.getJourney().getPortOfDepartureAtd())
                        .map(TrackingServiceApiResponse.DateAndSources::getDateTime).orElse(null));
            }
            return trackingEventsResponse;

        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public List<Events> generateEventsFromTrackingResponse(TrackingServiceApiResponse trackingServiceApiResponse) {
        List<Events> trackingEvents = new ArrayList<>();
        ConcurrentLinkedQueue<Events> eventsRows = new ConcurrentLinkedQueue<>();
        ForkJoinPool customThreadPool = new ForkJoinPool(4);

        try {
            if (trackingServiceApiResponse.getContainers() != null && !trackingServiceApiResponse.getContainers().isEmpty()) {
                customThreadPool.submit(() ->
                        trackingServiceApiResponse.getContainers().parallelStream().forEach(container -> {
                            if (container == null || container.getEvents() == null || container.getPlaces() == null) {
                                return;
                            }

                            // Mapping container events to Events list without processing sources
                            List<Events> rows = container.getEvents().stream()
                                    .filter(Objects::nonNull)
                                    .map(event -> {
                                        // Start building the Events object
                                        Events.EventsBuilder eventBuilder = Events.builder()
                                                .actual(Optional.ofNullable(event.getActualEventTime())
                                                        .map(DateAndSources::getDateTime).orElse(null))
                                                .estimated(Optional.ofNullable(event.getProjectedEventTime())
                                                        .map(DateAndSources::getDateTime).orElse(null))
                                                .eventCode(convertTrackingEventCodeToShortCode(event))
                                                .description(event.getDescription())
                                                .containerNumber(container.getContainerNumber())
                                                .locationRole(event.getLocationRole());

                                        // Populate fields from Places independently
                                        Optional.ofNullable(event.getLocation())
                                                .flatMap(locationId -> container.getPlaces().stream()
                                                        .filter(place -> place != null && locationId.equals(place.getId()))
                                                        .findFirst())
                                                .ifPresent(place -> eventBuilder.latitude(place.getLatitude())
                                                        .longitude(place.getLongitude())
                                                        .placeDescription(StringUtils.left(place.getFormattedDescription(), 100))
                                                        .placeName(place.getCode()));

                                        // Populate fields from Transports independently
                                        Optional.ofNullable(event.getDetails())
                                                .map(Details::getTransport)
                                                .flatMap(transportId -> container.getTransports().stream()
                                                        .filter(transport -> transport != null && transportId.equals(transport.getId()))
                                                        .findFirst())
                                                .ifPresent(transport -> eventBuilder.flightNumber(transport.getName())
                                                        .flightName(StringUtility.convertToString(transport.getOperatorName())));

                                        // Build and return the event
                                        return eventBuilder.build();
                                    })
                                    .toList();

                            eventsRows.addAll(rows);
                        })
                ).join();

                trackingEvents.addAll(eventsRows.stream().toList());
            }
        } finally {
            customThreadPool.shutdown();
        }
        return trackingEvents;
    }
}
