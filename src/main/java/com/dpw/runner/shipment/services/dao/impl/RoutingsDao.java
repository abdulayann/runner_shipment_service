package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IRoutingsRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class RoutingsDao implements IRoutingsDao {
    public static final String ROUTING_IS_NULL_FOR_ID_MSG = "Routing is null for Id {}";
    @Autowired
    private IRoutingsRepository routingsRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    private CommonUtils commonUtils;

    @Override
    public Routings save(Routings routings) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(routings), Constants.ROUTING, LifecycleHooks.ON_CREATE, false);
        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        validateRoutingForDocumentSelection(routings);
        return routingsRepository.save(routings);
    }
    @Override
    public List<Routings> saveAll(List<Routings> routingsList) {
        for(var routings: routingsList) {
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(routings), Constants.ROUTING, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(String.join(",", errors));
            validateRoutingForDocumentSelection(routings);
        }
        return routingsRepository.saveAll(routingsList);
    }

    @Override
    public Page<Routings> findAll(Specification<Routings> spec, Pageable pageable) {
        return routingsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Routings> findById(Long id) {
        return routingsRepository.findById(id);
    }

    @Override
    public Optional<Routings> findByGuid(UUID id) {
        return routingsRepository.findByGuid(id);
    }

    @Override
    public void delete(Routings routings) {
        routingsRepository.delete(routings);
    }

    @Override
    public List<Routings> updateEntityFromShipment(List<Routings> routingsList, Long shipmentId) throws RunnerException {
        String responseMsg;
        List<Routings> responseRoutings = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            List<Routings> routings = findByShipmentId(shipmentId);
            Map<Long, Routings> hashMap = routings.stream()
                        .collect(Collectors.toMap(Routings::getId, Function.identity()));
            Map<Long, Routings> copyHashMap = new HashMap<>(hashMap);
            List<Routings> routingsRequestList = new ArrayList<>();
            if (routingsList != null && !routingsList.isEmpty()) {
                for (Routings request : routingsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    routingsRequestList.add(request);
                }
                responseRoutings = saveEntityFromShipment(routingsRequestList, shipmentId, copyHashMap);
            }
            deleteRoutings(hashMap, ShipmentDetails.class.getSimpleName(), shipmentId);
            return responseRoutings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    private List<Routings> findByShipmentId(Long shipmentId) {
        return routingsRepository.findByShipmentId(shipmentId);
    }

    @Override
    public List<Routings> saveEntityFromShipment(List<Routings> routings, Long shipmentId) {
        List<Routings> res = new ArrayList<>();
        for (Routings req : routings) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                Optional<Routings> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug(ROUTING_IS_NULL_FOR_ID_MSG, req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
                oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
                operation = DBOperationType.UPDATE.name();
            }
            req.setShipmentId(shipmentId);
            req = save(req);
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Routings.class) : null)
                                .parent(ShipmentDetails.class.getSimpleName())
                                .parentId(shipmentId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
            res.add(req);
        }
        return res;
    }
    @Override
    public List<Routings> saveEntityFromShipment(List<Routings> routings, Long shipmentId, Map<Long, Routings> oldEntityMap) {
        List<Routings> res = new ArrayList<>();
        Map<Long, String> oldEntityJsonStringMap = new HashMap<>();
        for (Routings req : routings) {
            if (req.getId() != null) {
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug(ROUTING_IS_NULL_FOR_ID_MSG, req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntityMap.get(id).getCreatedAt());
                req.setCreatedBy(oldEntityMap.get(id).getCreatedBy());
                String oldEntityJsonString = jsonHelper.convertToJson(oldEntityMap.get(id));
                oldEntityJsonStringMap.put(id, oldEntityJsonString);
            }
            req.setShipmentId(shipmentId);
            res.add(req);
        }
        res = saveAll(res);
        for (Routings req : res) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if(oldEntityJsonStringMap.containsKey(req.getId())){
                oldEntityJsonString = oldEntityJsonStringMap.get(req.getId());
                operation = DBOperationType.UPDATE.name();
            }
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Routings.class) : null)
                                .parent(ShipmentDetails.class.getSimpleName())
                                .parentId(shipmentId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
        }
        return res;
    }

    @Override
    public List<Routings> updateEntityFromBooking(List<Routings> routingsList, Long bookingId) throws RunnerException {
        String responseMsg;
        List<Routings> responseRoutings = new ArrayList<>();
        try {
            ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
            Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
            Page<Routings> routings = findAll(pair.getLeft(), pair.getRight());
            Map<Long, Routings> hashMap = routings.stream()
                    .collect(Collectors.toMap(Routings::getId, Function.identity()));
            List<Routings> routingsRequestList = new ArrayList<>();
            if (routingsList != null && !routingsList.isEmpty()) {
                for (Routings request : routingsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    routingsRequestList.add(request);
                }
                responseRoutings = saveEntityFromBooking(routingsRequestList, bookingId);
            }
            deleteRoutings(hashMap, "CustomerBooking", bookingId);
            return responseRoutings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public List<Routings> saveEntityFromBooking(List<Routings> routings, Long bookingId) {
        List<Routings> res = new ArrayList<>();
        ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
        Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
        Page<Routings> routingsPage = findAll(pair.getLeft(), pair.getRight());
        Map<Long, Routings> hashMap = routingsPage.stream()
                .collect(Collectors.toMap(Routings::getId, Function.identity()));
        for (Routings req : routings) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                if (hashMap.get(id) == null) {
                    log.debug(ROUTING_IS_NULL_FOR_ID_MSG, req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                oldEntityJsonString = jsonHelper.convertToJson(hashMap.get(id));
                operation = DBOperationType.UPDATE.name();
            }
            req.setBookingId(bookingId);
            req = save(req);
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Routings.class) : null)
                                .parent(CustomerBooking.class.getSimpleName())
                                .parentId(bookingId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
            res.add(req);
        }
        return res;
    }

    public List<Routings> updateEntityFromConsole(List<Routings> routingsList, Long consolidationId) throws RunnerException {
        String responseMsg;
        List<Routings> responseRoutings = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            List<Routings> routings = findRoutingsByConsolidationId(consolidationId);
            Map<Long, Routings> hashMap = routings.stream()
                        .collect(Collectors.toMap(Routings::getId, Function.identity()));
            Map<Long, Routings> copyHashMap = new HashMap<>(hashMap);
            List<Routings> routingsRequestList = new ArrayList<>();
            if (routingsList != null && !routingsList.isEmpty()) {
                for (Routings request : routingsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    routingsRequestList.add(request);
                }
                responseRoutings = saveEntityFromConsole(routingsRequestList, consolidationId, copyHashMap);
            }
            deleteRoutings(hashMap, null, null);
            return responseRoutings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public List<Routings> updateEntityFromConsole(List<Routings> routingsList, Long consolidationId, List<Routings> oldEntityList) throws RunnerException {
        String responseMsg;
        Map<UUID, Routings> routingMap = new HashMap<>();
        if (oldEntityList != null && !oldEntityList.isEmpty()) {
            for (Routings entity :
                    oldEntityList) {
                routingMap.put(entity.getGuid(), entity);
            }
        }

        List<Routings> responseRoutings = new ArrayList<>();
        try {
            Routings oldEntity;
            List<Routings> routingsRequestList = new ArrayList<>();
            if (routingsList != null && !routingsList.isEmpty()) {
                for (Routings request : routingsList) {
                    oldEntity = routingMap.get(request.getGuid());
                    if (oldEntity != null) {
                        routingMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    routingsRequestList.add(request);
                }
                responseRoutings = saveEntityFromConsole(routingsRequestList, consolidationId);
            }
            Map<Long, Routings> hashMap = new HashMap<>();
            routingMap.forEach((s, routings) -> hashMap.put(routings.getId(), routings));
            deleteRoutings(hashMap, null, null);
            return responseRoutings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public List<Routings> saveEntityFromConsole(List<Routings> routings, Long consolidationId) {
        List<Routings> res = new ArrayList<>();
        for (Routings req : routings) {
            if (req.getId() != null) {
                long id = req.getId();
                Optional<Routings> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug(ROUTING_IS_NULL_FOR_ID_MSG, req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
            }
            req.setConsolidationId(consolidationId);
            req = save(req);
            res.add(req);
        }
        return res;
    }
    @Override
    public List<Routings> saveEntityFromConsole(List<Routings> routings, Long consolidationId, Map<Long, Routings> oldEntityMap) {
        List<Routings> res = new ArrayList<>();
        for (Routings req : routings) {
            if (req.getId() != null) {
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug(ROUTING_IS_NULL_FOR_ID_MSG, req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntityMap.get(id).getCreatedAt());
                req.setCreatedBy(oldEntityMap.get(id).getCreatedBy());
            }
            req.setConsolidationId(consolidationId);
            res.add(req);
        }
        res = saveAll(res);
        return res;
    }

    private void deleteRoutings(Map<Long, Routings> hashMap, String entity, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(routing -> {
                String json = jsonHelper.convertToJson(routing);
                delete(routing);
                if(entity != null)
                {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, Routings.class))
                                        .parent(entity)
                                        .parentId(entityId)
                                        .operation(DBOperationType.DELETE.name()).build()
                        );
                    } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                             InvocationTargetException | NoSuchMethodException | RunnerException e) {
                        log.error(e.getMessage());
                    }
                }
            });
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

    @Override
    public List<Routings> updateEntityFromShipment(List<Routings> routingsList, Long shipmentId, List<Routings> oldEntityList) throws RunnerException {
        String responseMsg;
        Map<UUID, Routings> routingMap = new HashMap<>();
        if (oldEntityList != null && !oldEntityList.isEmpty()) {
            for (Routings entity :
                    oldEntityList) {
                routingMap.put(entity.getGuid(), entity);
            }
        }

        List<Routings> responseRoutings = new ArrayList<>();
        try {
            Routings oldEntity;
            List<Routings> routingsRequestList = new ArrayList<>();
            if (routingsList != null && !routingsList.isEmpty()) {
                for (Routings request : routingsList) {
                    oldEntity = routingMap.get(request.getGuid());
                    if (oldEntity != null) {
                        routingMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    routingsRequestList.add(request);
                }
                responseRoutings = saveEntityFromShipment(routingsRequestList, shipmentId);
            }
            Map<Long, Routings> hashMap = new HashMap<>();
            routingMap.forEach((s, routings) -> hashMap.put(routings.getId(), routings));
            deleteRoutings(hashMap, null, null);
            return responseRoutings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    private void validateRoutingForDocumentSelection(Routings routings) {
        if (Boolean.TRUE.equals(routings.getIsSelectedForDocument()) &&
            (RoutingCarriage.PRE_CARRIAGE.equals(routings.getCarriage()) || RoutingCarriage.ON_CARRIAGE.equals(routings.getCarriage()))) {
            throw new ValidationException(Constants.ROUTING_VALIDATION);
        }
    }

    /**
     * Retrieves the list of routing requests for the specified customer booking request.
     *
     * <p>If the customer booking request already contains a routing list, it returns that list.
     * Otherwise, it generates routing legs based on the carrier details provided in the request.</p>
     *
     * <p>The routing legs are generated based on the following logic:</p>
     * <ul>
     *     <li>If Origin and Port of Loading (POL) are different, create a leg from Origin to POL.</li>
     *     <li>If POL and Port of Discharge (POD) are different, create a leg from POL to POD.</li>
     *     <li>If POD and Destination are different, create a leg from POD to Destination.</li>
     *     <li>If all points are the same (Origin, POL, POD, Destination), create a single leg from Origin to Destination.</li>
     * </ul>
     *
     * @return a list of {@link RoutingsRequest} containing the generated or existing routing legs
     */
    @Override
    public List<Routings> generateDefaultRouting(CarrierDetails carrier, String transportMode) {

        // Initialize the list to hold routing requests
        List<Routings> routingRequests = new ArrayList<>();

        CarrierDetails carrierDetails = Optional.ofNullable(carrier)
                .orElse(new CarrierDetails());

        // Define origin, ports, and destination with their respective transport modes
        Pair<String, String> originPort = Pair.of(carrierDetails.getOrigin(), Constants.TRANSPORT_MODE_ROA);
        Pair<String, String> portOfLoading = Pair.of(carrierDetails.getOriginPort(), null);
        Pair<String, String> portOfDischarge = Pair.of(carrierDetails.getDestinationPort(), null);
        Pair<String, String> destinationPort = Pair.of(carrierDetails.getDestination(), Constants.TRANSPORT_MODE_ROA);

        // Create a list of locations for processing
        List<Pair<String, String>> locations = List.of(originPort, portOfLoading, portOfDischarge, destinationPort);

        int currentLocation = 0; // Index for the current location
        int nextLocation = 1; // Index for the next location to compare
        long legCounter = 1L;   // A counter for leg numbers
        // Loop through the locations to generate routing requests
        while (canProcessNextLocation(currentLocation, nextLocation)) {
            // Skip null locations or If locations are the same, move to the next pair
            if (skipSameLocation(locations, currentLocation, nextLocation)) {
                currentLocation++;
                nextLocation++;
            } else if (locations.get(nextLocation).getLeft() == null) {
                nextLocation++;
            } else {
                String mode = transportMode;
                RoutingCarriage carriage = RoutingCarriage.MAIN_CARRIAGE;

                if (canUpdateModeCarriage(locations, currentLocation, nextLocation)) {
                    mode = Constants.TRANSPORT_MODE_ROA; // Set mode to ROA if specific conditions are met
                    carriage = locations.get(currentLocation).getRight() != null  ? RoutingCarriage.PRE_CARRIAGE : RoutingCarriage.ON_CARRIAGE;
                }
                String flightNumber = "";
                String flightCarrier = "";
                if(shouldIncludeFlightDetails(transportMode, carriage)) {
                    flightNumber = carrierDetails.getFlightNumber();
                    flightCarrier = carrierDetails.getShippingLine();
                }

                // Create and add a new routing request to the list
                routingRequests.add(createRoutingsRequest(legCounter++, mode, locations.get(currentLocation).getLeft(), locations.get(nextLocation).getLeft(), carriage, flightNumber, flightCarrier));
                currentLocation = nextLocation;
                nextLocation++;
            }
        }

        // Return the generated routing requests
        return routingRequests;
    }

    private boolean canProcessNextLocation(int currentLocation, int nextLocation) {
        return currentLocation < 4 && nextLocation < 4;
    }

    private boolean canUpdateModeCarriage(List<Pair<String, String>> locations, int currentLocation, int nextLocation) {
        return locations.get(currentLocation).getRight() != null || locations.get(nextLocation).getRight() != null;
    }

    private boolean skipSameLocation(List<Pair<String, String>> locations, int currentLocation, int nextLocation) {
        return locations.get(currentLocation).getLeft() == null || locations.get(currentLocation).getLeft().equalsIgnoreCase(locations.get(nextLocation).getLeft());
    }

    private boolean shouldIncludeFlightDetails(String transportMode, RoutingCarriage carriage) {
        return Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster()) &&
                Objects.equals(transportMode, Constants.TRANSPORT_MODE_AIR) &&
                Objects.equals(carriage, RoutingCarriage.MAIN_CARRIAGE);
    }

    /**
     * Creates a new routing request.
     *
     * @param leg   the leg number for the routing request
     * @param mode  the mode of transport for the routing request
     * @param pol   the Port of Loading for the routing request
     * @param pod   the Port of Discharge for the routing request
     * @return a new {@link RoutingsRequest} object with the specified parameters
     */
    private Routings createRoutingsRequest(Long leg, String mode, String pol, String pod, RoutingCarriage carriage, String flightNumber, String carrier) {
        // Build and return the RoutingsRequest object with the given parameters
        return Routings.builder()
                .leg(leg)
                .mode(mode)
                .pol(pol)
                .pod(pod)
                .carrier(carrier)
                .flightNumber(flightNumber)
                .carriage(carriage)
                .isSelectedForDocument(false)
                .isDomestic(false)
                .build();
    }

    @Override
    public List<Routings> findRoutingsByConsolidationId(Long consolidationId) {
        return routingsRepository.findByConsolidationId(consolidationId);
    }
}
