package com.dpw.runner.shipment.services.migration.strategy.impl;


import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.impl.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RestoreFailureException;
import com.dpw.runner.shipment.services.migration.dao.impl.ShipmentBackupDao;
import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.RestoreServiceHandler;
import com.dpw.runner.shipment.services.repository.interfaces.IBookingCarriageRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IELDetailsRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IEventRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IJobRepository;
import com.dpw.runner.shipment.services.repository.interfaces.INotesRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPackingRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPartiesRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPickupDeliveryDetailsRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IReferenceNumbersRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IRoutingsRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IServiceDetailsRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentOrderRepository;
import com.dpw.runner.shipment.services.repository.interfaces.ITruckDriverDetailsRepository;
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
import java.util.stream.Collectors;

@Service
@Slf4j
public class ShipmentRestoreHandler implements RestoreServiceHandler {

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private PackingDao packingDao;

    @Autowired
    private IPackingRepository packingRepository;

    @Autowired
    private ReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private IReferenceNumbersRepository referenceNumbersRepository;

    @Autowired
    private RoutingsDao routingsDao;

    @Autowired
    private IRoutingsRepository routingsRepository;

    @Autowired
    private ContainerDao containerDao;

    @Autowired
    private EventDao eventDao;

    @Autowired
    private IEventRepository eventRepository;

    @Autowired
    private PartiesDao partiesDao;

    @Autowired
    private IPartiesRepository partiesRepository;

    @Autowired
    private IJobRepository iJobRepository;

    @Autowired
    private ShipmentBackupDao shipmentBackupDao;

    @Autowired
    private BookingCarriageDao bookingCarriageDao;

    @Autowired
    private IBookingCarriageRepository bookingCarriageRepository;

    @Autowired
    private ELDetailsDao elDetailsDao;

    @Autowired
    private IELDetailsRepository ielDetailsRepository;

    @Autowired
    private ServiceDetailsDao serviceDetailsDao;

    @Autowired
    private IServiceDetailsRepository serviceDetailsRepository;

    @Autowired
    private TruckDriverDetailsDao truckDriverDetailsDao;

    @Autowired
    private ITruckDriverDetailsRepository iTruckDriverDetailsRepository;

    @Autowired
    private NotesDao notesDao;

    @Autowired
    private INotesRepository notesRepository;

    @Autowired
    private ShipmentOrderDao shipmentOrderDao;

    @Autowired
    private IShipmentOrderRepository shipmentOrderRepository;

    @Autowired
    private PickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    @Autowired
    private IPickupDeliveryDetailsRepository pickupDeliveryDetailsRepository;

    @Autowired
    private ShipmentDao shipmentDao;

    @Autowired
    @Qualifier("rollbackTaskExecutor")
    private ThreadPoolTaskExecutor rollbackTaskExecutor;
    @Autowired
    private ShipmentsContainersMappingDao shipmentsContainersMappingDao;

    public ShipmentDetails restoreShipmentDetails(Long shipmentId, Map<Long, List<Long>> containerShipmentMap, ConsolidationDetails consolidationDetails) throws JsonProcessingException {

        log.info("Starting shipment restore for shipmentId: {}", shipmentId);
        ShipmentBackupEntity shipmentBackupDetails = shipmentBackupDao.findByShipmentId(shipmentId);
        Set<Long> shipmentsContainersMapping = new HashSet<>(shipmentsContainersMappingDao.findByShipmentId(shipmentId).stream().map(ShipmentsContainersMapping::getContainerId).toList());
        if (null == shipmentBackupDetails) {
            log.info("No Shipment records found for ShipmentId: {}", shipmentId);
            return null;
        }
        ShipmentDetails shipmentDetails = objectMapper.readValue(shipmentBackupDetails.getShipmentDetail(), ShipmentDetails.class);
        if (consolidationDetails != null) {
            shipmentDetails.setConsolidationList(Set.of(consolidationDetails));
        }
        processContainerToShipmentMapping(shipmentId, shipmentDetails, containerShipmentMap);

        var containerList = shipmentDetails.getContainersList().stream().filter(x -> shipmentsContainersMapping.contains(x.getId())).collect(Collectors.toSet());
        shipmentDetails.setContainersList(containerList);
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
        shipmentBackupDao.makeIsDeleteTrueToMarkRestoreSuccessful(shipmentBackupDetails.getId());

        return shipmentDetails;
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
        pickupDeliveryDetailsRepository.saveAll(shipmentDetails.getPickupDeliveryDetailsInstructions());
    }

    private void validateAndSetShipmentOrderDetails(Long shipmentId, List<Long> shipmentOrderIds, ShipmentDetails shipmentDetails) {
        shipmentOrderDao.deleteAdditionalShipmentOrderByShipmentId(shipmentOrderIds, shipmentId);
        shipmentOrderDao.revertSoftDeleteByshipmentOrderIdsAndShipmentId(shipmentOrderIds, shipmentId);
        shipmentOrderRepository.saveAll(shipmentDetails.getShipmentOrders());
    }

    private void validateAndSetPartiesDetails(Long shipmentId, List<Long> partiesIds, ShipmentDetails shipmentDetails) {
        partiesDao.deleteAdditionalPartiesByEntityIdAndEntityType(partiesIds, shipmentId, Constants.SHIPMENT_ADDRESSES);
        partiesDao.revertSoftDeleteByPartiesIdsAndEntityIdAndEntityType(partiesIds, shipmentId, Constants.SHIPMENT_ADDRESSES);
        partiesRepository.saveAll(shipmentDetails.getShipmentAddresses());
    }

    private void validateAndSetJobsDetails(Long shipmentId, List<Long> jobsIds, ShipmentDetails shipmentDetails) {
        iJobRepository.deleteAdditionalJobsByShipmentId(jobsIds, shipmentId);
        iJobRepository.revertSoftDeleteByJobsIdsAndShipmentId(jobsIds, shipmentId);
        iJobRepository.saveAll(shipmentDetails.getJobsList());
    }

    private void validateAndSetNotesDetails(Long shipmentId, List<Long> notesIds, ShipmentDetails shipmentDetails) {
        notesDao.deleteAdditionalNotesByEntityIdAndEntityType(notesIds, shipmentId, Constants.SHIPMENT);
        notesDao.revertSoftDeleteByNotesIdsAndEntityIdAndEntityType(notesIds, shipmentId, Constants.SHIPMENT);
        notesRepository.saveAll(shipmentDetails.getNotesList());
    }

    private void validateAndSetTruckDriverDetails(Long shipmentId, List<Long> truckDriverDetailsIds, ShipmentDetails shipmentDetails) {
        truckDriverDetailsDao.deleteAdditionalTruckDriverDetailsByShipmentId(truckDriverDetailsIds, shipmentId);
        truckDriverDetailsDao.revertSoftDeleteByTruckDriverDetailsIdsAndShipmentId(truckDriverDetailsIds, shipmentId);
        iTruckDriverDetailsRepository.saveAll(shipmentDetails.getTruckDriverDetails());
    }

    private void validateAndSetServiceDetails(Long shipmentId, List<Long> serviceDetailsIds, ShipmentDetails shipmentDetails) {
        serviceDetailsDao.deleteAdditionalServiceDetailsByShipmentId(serviceDetailsIds, shipmentId);
        serviceDetailsDao.revertSoftDeleteByServiceDetailsIdsAndShipmentId(serviceDetailsIds, shipmentId);
        serviceDetailsRepository.saveAll(shipmentDetails.getServicesList());
    }

    private void validateAndSetRoutingsIds(Long shipmentId, List<Long> routingsIds, ShipmentDetails shipmentDetails) {
        routingsDao.deleteAdditionalroutingsByShipmentId(routingsIds, shipmentId);
        routingsDao.revertSoftDeleteByroutingsIdsAndShipmentId(routingsIds, shipmentId);
        routingsRepository.saveAll(shipmentDetails.getRoutingsList());
    }

    private void validateAndSetReferenceNumbersDetails(Long shipmentId, List<Long> referenceNumbersIds, ShipmentDetails shipmentDetails) {
        referenceNumbersDao.deleteAdditionalreferenceNumbersByShipmentId(referenceNumbersIds, shipmentId);
        referenceNumbersDao.revertSoftDeleteByreferenceNumbersIdsAndShipmentId(referenceNumbersIds, shipmentId);
        referenceNumbersRepository.saveAll(shipmentDetails.getReferenceNumbersList());
    }

    private void validateAndSetEventsDetails(Long shipmentId, List<Long> eventsIds, ShipmentDetails shipmentDetails) {
        eventDao.deleteAdditionalEventDetailsByEntityIdAndEntityType(eventsIds, shipmentId, Constants.SHIPMENT);
        eventDao.revertSoftDeleteByEventDetailsIdsAndEntityIdAndEntityType(eventsIds, shipmentId, Constants.SHIPMENT);
        eventRepository.saveAll(shipmentDetails.getEventsList());
    }

    private void validateAndSetElDetails(Long shipmentId, List<Long> elDetailsIds, ShipmentDetails shipmentDetails) {
        elDetailsDao.deleteAdditionalElDetailsByShipmentId(elDetailsIds, shipmentId);
        elDetailsDao.revertSoftDeleteByElDetailsIdsAndShipmentId(elDetailsIds, shipmentId);
        ielDetailsRepository.saveAll(shipmentDetails.getElDetailsList());
    }

    private void validateAndSetBookingCarriagesDetails(Long shipmentId, List<Long> bookingCarriageIds, ShipmentDetails shipmentDetails) {
        bookingCarriageDao.deleteAdditionalbookingCarriageByShipmentId(bookingCarriageIds, shipmentId);
        bookingCarriageDao.revertSoftDeleteBybookingCarriageIdsAndShipmentId(bookingCarriageIds, shipmentId);
        bookingCarriageRepository.saveAll(shipmentDetails.getBookingCarriagesList());
    }

    private void validateAndSetPackingDetails(Long shipmentId, List<Long> packingIds, ShipmentDetails shipmentDetails) {
        packingDao.deleteAdditionalPackingByShipmentId(packingIds, shipmentId);
        packingDao.revertSoftDeleteByPackingIdsAndShipmentId(packingIds, shipmentId);
        packingRepository.saveAll(shipmentDetails.getPackingList());
    }

    @Override
    public void restore(Integer tenantId) {

        Set<Long> allBackupShipmentIds = shipmentBackupDao.findShipmentIdsByTenantId(tenantId);
        if (allBackupShipmentIds.isEmpty()) {
            return;
        }
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
                            restoreShipmentDetails(shipmentId, null, null);
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
