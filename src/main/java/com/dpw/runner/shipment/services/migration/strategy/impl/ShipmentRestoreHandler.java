package com.dpw.runner.shipment.services.migration.strategy.impl;


import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.impl.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RestoreFailureException;
import com.dpw.runner.shipment.services.migration.dao.impl.ShipmentBackupDao;
import com.dpw.runner.shipment.services.migration.dao.interfaces.IShipmentBackupDao;
import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.RestoreHandler;
import com.dpw.runner.shipment.services.repository.interfaces.IJobRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.CompletableFuture;

@Service
@Slf4j
public class ShipmentRestoreHandler implements RestoreHandler {

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private PackingDao packingDao;

    @Autowired
    private ReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private RoutingsDao routingsDao;

    @Autowired
    private ContainerDao containerDao;

    @Autowired
    private EventDao eventDao;

    @Autowired
    private PartiesDao partiesDao;

    @Autowired
    private IJobRepository iJobRepository;

    @Autowired
    private ShipmentBackupDao shipmentBackupDao;

    @Autowired
    private BookingCarriageDao bookingCarriageDao;

    @Autowired
    private ELDetailsDao elDetailsDao;

    @Autowired
    private ServiceDetailsDao serviceDetailsDao;

    @Autowired
    private TruckDriverDetailsDao truckDriverDetailsDao;

    @Autowired
    private NotesDao notesDao;

    @Autowired
    private ShipmentOrderDao shipmentOrderDao;

    @Autowired
    private PickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    @Autowired
    private ShipmentDao shipmentDao;

    @Autowired
    @Qualifier("rollbackTaskExecutor")
    private ThreadPoolTaskExecutor rollbackTaskExecutor;

    public void restoreShipmentDetails(Long shipmentId, Map<Long, List<Long>> containerShipmentMap) throws JsonProcessingException {

        log.info("Starting shipment restore for shipmentId: {}", shipmentId);
        ShipmentBackupEntity shipmentBackupDetails = shipmentBackupDao.findByShipmentId(shipmentId);
        if (null == shipmentBackupDetails) {
            log.info("No Shipment records found for ShipmentId: {}", shipmentId);
            return;
        }
        ShipmentDetails shipmentDetails = objectMapper.readValue(shipmentBackupDetails.getShipmentDetail(), ShipmentDetails.class);
        processContainerToShipmentMapping(shipmentId, shipmentDetails, containerShipmentMap);
        List<Long> packingIds = shipmentDetails.getPackingList().stream().map(Packing::getId).filter(Objects::nonNull).toList();
        validateAndSetPackingDetails(shipmentId, packingIds, shipmentDetails);
        List<Long> bookingCarriageIds = shipmentDetails.getBookingCarriagesList().stream().map(BookingCarriage::getId).filter(Objects::nonNull).toList();
        validateAndSetBookingCarriagesDetails(shipmentId, bookingCarriageIds, shipmentDetails);
        List<Long> elDetailsIds = shipmentDetails.getElDetailsList().stream().map(ELDetails::getId).filter(Objects::nonNull).toList();
        validateAndSetElDetails(shipmentId, elDetailsIds, shipmentDetails);
        List<Long> eventsIds = shipmentDetails.getEventsList().stream().map(Events::getId).filter(Objects::nonNull).toList();
        validateAndSetEventsDetails(shipmentId, eventsIds, shipmentDetails);
        List<Long> referenceNumbersIds = shipmentDetails.getReferenceNumbersList().stream().map(ReferenceNumbers::getId).filter(Objects::nonNull).toList();
        validateAndSetReferenceNumbersDetails(shipmentId, referenceNumbersIds, shipmentDetails);
        List<Long> routingsIds = shipmentDetails.getRoutingsList().stream().map(Routings::getId).filter(Objects::nonNull).toList();
        validateAndSetRoutingsIds(shipmentId, routingsIds, shipmentDetails);
        List<Long> serviceDetailsIds = shipmentDetails.getServicesList().stream().map(ServiceDetails::getId).filter(Objects::nonNull).toList();
        validateAndSetServiceDetails(shipmentId, serviceDetailsIds, shipmentDetails);
        List<Long> truckDriverDetailsIds = shipmentDetails.getTruckDriverDetails().stream().map(TruckDriverDetails::getId).filter(Objects::nonNull).toList();
        validateAndSetTruckDriverDetails(shipmentId, truckDriverDetailsIds, shipmentDetails);
        List<Long> notesIds = shipmentDetails.getNotesList().stream().map(Notes::getId).filter(Objects::nonNull).toList();
        validateAndSetNotesDetails(shipmentId, notesIds, shipmentDetails);
        List<Long> jobsIds = shipmentDetails.getJobsList().stream().map(Jobs::getId).filter(Objects::nonNull).toList();
        validateAndSetJobsDetails(shipmentId, jobsIds, shipmentDetails);
        List<Long> partiesIds = shipmentDetails.getShipmentAddresses().stream().map(Parties::getId).filter(Objects::nonNull).toList();
        validateAndSetPartiesDetails(shipmentId, partiesIds, shipmentDetails);
        List<Long> shipmentOrderIds = shipmentDetails.getShipmentOrders().stream().map(ShipmentOrder::getId).filter(Objects::nonNull).toList();
        validateAndSetShipmentOrderDetails(shipmentId, shipmentOrderIds, shipmentDetails);
        List<Long> pickupDeliveryDetailsIds = shipmentDetails.getPickupDeliveryDetailsInstructions().stream().map(PickupDeliveryDetails::getId).filter(Objects::nonNull).toList();
        validateAndSetPickupDeliveryDetails(shipmentId, pickupDeliveryDetailsIds, shipmentDetails);
        shipmentDao.saveWithoutValidation(shipmentDetails);
    }

    private void processContainerToShipmentMapping(Long shipmentId, ShipmentDetails shipmentDetails, Map<Long, List<Long>> containerShipmentMap) {
        if (shipmentDetails.getContainersList() != null) {
            for (Containers container : shipmentDetails.getContainersList()) {
                Long containerId = container.getId();
                if (containerId != null) {
                    containerShipmentMap.computeIfAbsent(containerId, k -> new ArrayList<>()).add(shipmentId);
                }
            }
        }
    }

    private void validateAndSetPickupDeliveryDetails(Long shipmentId, List<Long> pickupDeliveryDetailsIds, ShipmentDetails shipmentDetails) {
        pickupDeliveryDetailsDao.deleteAdditionalPickupDeliveryDetailsByShipmentId(pickupDeliveryDetailsIds, shipmentId);
        pickupDeliveryDetailsDao.revertSoftDeleteByPickupDeliveryDetailsIdsAndShipmentId(pickupDeliveryDetailsIds, shipmentId);
        for (PickupDeliveryDetails restored : shipmentDetails.getPickupDeliveryDetailsInstructions()) {
            pickupDeliveryDetailsDao.save(restored);
        }
    }

    private void validateAndSetShipmentOrderDetails(Long shipmentId, List<Long> shipmentOrderIds, ShipmentDetails shipmentDetails) {
        shipmentOrderDao.deleteAdditionalShipmentOrderByShipmentId(shipmentOrderIds, shipmentId);
        shipmentOrderDao.revertSoftDeleteByshipmentOrderIdsAndShipmentId(shipmentOrderIds, shipmentId);
        for (ShipmentOrder restored : shipmentDetails.getShipmentOrders()) {
            shipmentOrderDao.save(restored);
        }
    }

    private void validateAndSetPartiesDetails(Long shipmentId, List<Long> partiesIds, ShipmentDetails shipmentDetails) {
        partiesDao.deleteAdditionalPartiesByEntityIdAndEntityType(partiesIds, shipmentId, Constants.SHIPMENT_ADDRESSES);
        partiesDao.revertSoftDeleteByPartiesIdsAndEntityIdAndEntityType(partiesIds, shipmentId, Constants.SHIPMENT_ADDRESSES);
        for (Parties restored : shipmentDetails.getShipmentAddresses()) {
            partiesDao.save(restored);
        }
    }

    private void validateAndSetJobsDetails(Long shipmentId, List<Long> jobsIds, ShipmentDetails shipmentDetails) {
        iJobRepository.deleteAdditionalJobsByShipmentId(jobsIds, shipmentId);
        iJobRepository.revertSoftDeleteByJobsIdsAndShipmentId(jobsIds, shipmentId);
        for (Jobs restored : shipmentDetails.getJobsList()) {
            iJobRepository.save(restored);
        }
    }

    private void validateAndSetNotesDetails(Long shipmentId, List<Long> notesIds, ShipmentDetails shipmentDetails) {
        notesDao.deleteAdditionalNotesByEntityIdAndEntityType(notesIds, shipmentId, Constants.SHIPMENT);
        notesDao.revertSoftDeleteByNotesIdsAndEntityIdAndEntityType(notesIds, shipmentId, Constants.SHIPMENT);
        for (Notes restored : shipmentDetails.getNotesList()) {
            notesDao.save(restored);
        }
    }

    private void validateAndSetTruckDriverDetails(Long shipmentId, List<Long> truckDriverDetailsIds, ShipmentDetails shipmentDetails) {
        truckDriverDetailsDao.deleteAdditionalTruckDriverDetailsByShipmentId(truckDriverDetailsIds, shipmentId);
        truckDriverDetailsDao.revertSoftDeleteByTruckDriverDetailsIdsAndShipmentId(truckDriverDetailsIds, shipmentId);
        for (TruckDriverDetails restored : shipmentDetails.getTruckDriverDetails()) {
            truckDriverDetailsDao.save(restored);
        }
    }

    private void validateAndSetServiceDetails(Long shipmentId, List<Long> serviceDetailsIds, ShipmentDetails shipmentDetails) {
        serviceDetailsDao.deleteAdditionalServiceDetailsByShipmentId(serviceDetailsIds, shipmentId);
        serviceDetailsDao.revertSoftDeleteByServiceDetailsIdsAndShipmentId(serviceDetailsIds, shipmentId);
        for (ServiceDetails restored : shipmentDetails.getServicesList()) {
            serviceDetailsDao.save(restored);
        }
    }

    private void validateAndSetRoutingsIds(Long shipmentId, List<Long> routingsIds, ShipmentDetails shipmentDetails) {
        routingsDao.deleteAdditionalroutingsByShipmentId(routingsIds, shipmentId);
        routingsDao.revertSoftDeleteByroutingsIdsAndShipmentId(routingsIds, shipmentId);
        for (Routings restored : shipmentDetails.getRoutingsList()) {
            routingsDao.save(restored);
        }
    }

    private void validateAndSetReferenceNumbersDetails(Long shipmentId, List<Long> referenceNumbersIds, ShipmentDetails shipmentDetails) {
        referenceNumbersDao.deleteAdditionalreferenceNumbersByShipmentId(referenceNumbersIds, shipmentId);
        referenceNumbersDao.revertSoftDeleteByreferenceNumbersIdsAndShipmentId(referenceNumbersIds, shipmentId);
        for (ReferenceNumbers restored : shipmentDetails.getReferenceNumbersList()) {
            referenceNumbersDao.save(restored);
        }
    }

    private void validateAndSetEventsDetails(Long shipmentId, List<Long> eventsIds, ShipmentDetails shipmentDetails) {
        eventDao.deleteAdditionalEventDetailsByEntityIdAndEntityType(eventsIds, shipmentId, Constants.SHIPMENT);
        eventDao.revertSoftDeleteByEventDetailsIdsAndEntityIdAndEntityType(eventsIds, shipmentId, Constants.SHIPMENT);
        for (Events restored : shipmentDetails.getEventsList()) {
            eventDao.save(restored);
        }
    }

    private void validateAndSetElDetails(Long shipmentId, List<Long> elDetailsIds, ShipmentDetails shipmentDetails) {
        elDetailsDao.deleteAdditionalElDetailsByShipmentId(elDetailsIds, shipmentId);
        elDetailsDao.revertSoftDeleteByElDetailsIdsAndShipmentId(elDetailsIds, shipmentId);
        for (ELDetails restored : shipmentDetails.getElDetailsList()) {
            elDetailsDao.save(restored);
        }
    }

    private void validateAndSetBookingCarriagesDetails(Long shipmentId, List<Long> bookingCarriageIds, ShipmentDetails shipmentDetails) {
        bookingCarriageDao.deleteAdditionalbookingCarriageByShipmentId(bookingCarriageIds, shipmentId);
        bookingCarriageDao.revertSoftDeleteBybookingCarriageIdsAndShipmentId(bookingCarriageIds, shipmentId);
        for (BookingCarriage restored : shipmentDetails.getBookingCarriagesList()) {
            bookingCarriageDao.save(restored);
        }
    }

    private void validateAndSetPackingDetails(Long shipmentId, List<Long> packingIds, ShipmentDetails shipmentDetails) {
        packingDao.deleteAdditionalPackingByShipmentId(packingIds, shipmentId);
        packingDao.revertSoftDeleteByPackingIdsAndShipmentId(packingIds, shipmentId);
        for (Packing restored : shipmentDetails.getPackingList()) {
            packingDao.save(restored);
        }
    }

    @Override
    public void restore(Integer tenantId) {

        Set<Long> allBackupShipmentIds = shipmentBackupDao.findShipmentIdsByTenantId(tenantId);
        Set<Long> allOriginalShipmentIds = shipmentDao.findAllShipmentIdsByTenantId(tenantId);

        // Soft delete bookings not present in backup
        Set<Long> idsToDelete = Sets.difference(allBackupShipmentIds, allOriginalShipmentIds);
        if (!idsToDelete.isEmpty()) {
            shipmentDao.deleteShipmentDetailsByIds(idsToDelete);
        }

        Set<Long> nonAttachedShipmentIds = shipmentBackupDao.findNonAttachedShipmentIdsByTenantId(tenantId);

        Lists.partition(new ArrayList<>(nonAttachedShipmentIds), 100).forEach(batch -> {
            List<CompletableFuture<Void>> futures = batch.stream().map(shipmentId ->
                    CompletableFuture.runAsync(() -> {
                        try {
                            restoreShipmentDetails(shipmentId, null);
                        } catch (Exception e) {
                            log.error("Failed shipment restore {}: {}", shipmentId, e.getMessage());
                            throw new RestoreFailureException("Rollback failed", e);
                        }
                    }, rollbackTaskExecutor)).toList();

            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).exceptionally(ex -> {
                log.error("Batch failed", ex);
                return null;
            }).join();
        });
    }
}
