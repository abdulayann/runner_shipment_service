package com.dpw.runner.shipment.services.migration.strategy.impl;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.impl.*;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RestoreFailureException;
import com.dpw.runner.shipment.services.migration.dao.impl.ShipmentBackupDao;
import com.dpw.runner.shipment.services.migration.entity.ShipmentBackupEntity;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.RestoreServiceHandler;
import com.dpw.runner.shipment.services.repository.interfaces.*;
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
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Sets;
import lombok.Generated;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;


import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
@Slf4j
@RequiredArgsConstructor
@Generated
public class ShipmentRestoreHandler implements RestoreServiceHandler {

    private final ObjectMapper objectMapper;
    private final PackingDao packingDao;
    private final IPackingRepository packingRepository;
    private final ReferenceNumbersDao referenceNumbersDao;
    private final IReferenceNumbersRepository referenceNumbersRepository;
    private final RoutingsDao routingsDao;
    private final IRoutingsRepository routingsRepository;
    private final ContainerDao containerDao;
    private final EventDao eventDao;
    private final IEventRepository eventRepository;
    private final PartiesDao partiesDao;
    private final IPartiesRepository partiesRepository;
    private final IJobRepository iJobRepository;
    private final ShipmentBackupDao shipmentBackupDao;
    private final BookingCarriageDao bookingCarriageDao;
    private final IBookingCarriageRepository bookingCarriageRepository;
    private final ELDetailsDao elDetailsDao;
    private final IELDetailsRepository ielDetailsRepository;
    private final ServiceDetailsDao serviceDetailsDao;
    private final IServiceDetailsRepository serviceDetailsRepository;
    private final TruckDriverDetailsDao truckDriverDetailsDao;
    private final ITruckDriverDetailsRepository iTruckDriverDetailsRepository;
    private final NotesDao notesDao;
    private final INotesRepository notesRepository;
    private final ShipmentOrderDao shipmentOrderDao;
    private final IShipmentOrderRepository shipmentOrderRepository;
    private final PickupDeliveryDetailsDao pickupDeliveryDetailsDao;
    private final IPickupDeliveryDetailsRepository pickupDeliveryDetailsRepository;
    private final ShipmentDao shipmentDao;
    private final ShipmentsContainersMappingDao shipmentsContainersMappingDao;
    private NetworkTransferDao networkTransferDao;
    private INetworkTransferRepository networkTransferRepository;
    private final V1ServiceImpl v1Service;


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
        List<NetworkTransfer> networkTransferList = shipmentBackupDetails.getNetworkTransferDetails() != null && !shipmentBackupDetails.getNetworkTransferDetails().isEmpty() ? objectMapper.readValue(shipmentBackupDetails.getNetworkTransferDetails(), new TypeReference<List<NetworkTransfer>>() {
        }) : new ArrayList<>();
        processContainerToShipmentMapping(shipmentId, shipmentDetails, containerShipmentMap);

        var containerList = shipmentDetails.getContainersList().stream().filter(x -> shipmentsContainersMapping.contains(x.getId())).collect(Collectors.toSet());
        shipmentDetails.setContainersList(containerList);
        List<Long> partiesIds = getAllPartiesIds(shipmentDetails);
        validateAndSetPartiesDetails(shipmentId, partiesIds, shipmentDetails);
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


        List<Long> shipmentOrderIds = shipmentDetails.getShipmentOrders().stream().map(ShipmentOrder::getId).filter(Objects::nonNull).toList();
        validateAndSetShipmentOrderDetails(shipmentId, shipmentOrderIds, shipmentDetails);
        List<Long> pickupDeliveryDetailsIds = shipmentDetails.getPickupDeliveryDetailsInstructions().stream().map(PickupDeliveryDetails::getId).filter(Objects::nonNull).toList();
        validateAndSetPickupDeliveryDetails(shipmentId, pickupDeliveryDetailsIds, shipmentDetails);
        validateAndSetNetworkTransferDetails(networkTransferList, shipmentDetails.getId());
        validateAndRestoreTriangularPartnerDetails(shipmentId);
        shipmentDao.saveWithoutValidation(shipmentDetails);
        shipmentBackupDao.makeIsDeleteTrueToMarkRestoreSuccessful(shipmentBackupDetails.getId());

        return shipmentDetails;
    }

    private static List<Long> getAllPartiesIds(ShipmentDetails shipmentDetails) {
        return Stream.of(
                safeStream(shipmentDetails.getServicesList()).map(ServiceDetails::getContractor),
                safeStream(shipmentDetails.getTruckDriverDetails()).map(TruckDriverDetails::getThirdPartyTransporter),
                safeStream(shipmentDetails.getJobsList()).flatMap(job -> job == null ? Stream.empty() : Stream.of(job.getBuyerDetail(), job.getSupplierDetail())),
                safeStream(shipmentDetails.getContainersList()).flatMap(container -> container == null ? Stream.empty() : Stream.of(container.getPickupAddress(), container.getDeliveryAddress())),
                safeStream(shipmentDetails.getPickupDeliveryDetailsInstructions())
                        .flatMap(pickupDelivery -> {
                            if (pickupDelivery == null) {
                                return Stream.empty();
                            }
                            return Stream.concat(
                                    Stream.of(
                                            pickupDelivery.getTransporterDetail(),
                                            pickupDelivery.getBrokerDetail(),
                                            pickupDelivery.getDestinationDetail(),
                                            pickupDelivery.getSourceDetail(),
                                            pickupDelivery.getAgentDetail()
                                    ),
                                    safeStream(pickupDelivery.getPartiesList())
                            );
                        }),
                safeStream(shipmentDetails.getShipmentAddresses())).flatMap(Function.identity()).filter(Objects::nonNull).map(parties -> parties == null ? null : parties.getId()).filter(Objects::nonNull).distinct().collect(Collectors.toList());
    }

    private static <T> Stream<T> safeStream(Collection<T> collection) {
        return collection == null ? Stream.empty() : collection.stream();
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

    private void validateAndSetNetworkTransferDetails(List<NetworkTransfer> networkTransferList, Long shipmentId) {
        List<NetworkTransfer> networkTransferDbList = networkTransferDao.findByEntityNTList(shipmentId, Constants.SHIPMENT);

        List<NetworkTransfer> toSaveList = new ArrayList<>();

        // Map of DB: id -> NetworkTransfer (keep all entries, don't filter upfront)
        Map<Long, NetworkTransfer> dbMap = networkTransferDbList.stream()
                .filter(nt -> nt.getId() != null)
                .collect(Collectors.toMap(NetworkTransfer::getId, nt -> nt));

        for (NetworkTransfer incoming : networkTransferList) {
            Long id = incoming.getId();
            UUID guid = incoming.getGuid();

            if (id != null && guid != null && dbMap.containsKey(id)) {
                NetworkTransfer existing = dbMap.get(id);
                if (existing.getGuid() != null && existing.getGuid().equals(guid)) {
                    // Valid match → update
                    toSaveList.add(incoming);
                    dbMap.remove(id); // remove matched entry
                    continue;
                }
            }

            // New record (or mismatched guid): remove id and save
            incoming.setId(null);
            toSaveList.add(incoming);
        }

        // All remaining in dbMap are to be deleted
        List<Long> toDeleteIds = new ArrayList<>(dbMap.keySet());

        // Persist
        networkTransferRepository.saveAll(toSaveList);
        networkTransferRepository.deleteAllById(toDeleteIds);
    }


    private void validateAndSetPickupDeliveryDetails(Long shipmentId, List<Long> pickupDeliveryDetailsIdsList, ShipmentDetails shipmentDetails) {
        List<Long> pickupDeliveryDetailsIds = ensureNonEmptyIds(pickupDeliveryDetailsIdsList);
        pickupDeliveryDetailsDao.deleteAdditionalPickupDeliveryDetailsByShipmentId(pickupDeliveryDetailsIds, shipmentId);
        List<Long> pickupDeliveryDetailsPartiesIdsList = shipmentDetails.getPickupDeliveryDetailsInstructions().stream()
                .filter(Objects::nonNull).flatMap(pickupDelivery -> pickupDelivery.getPartiesList() == null ? Stream.empty() : pickupDelivery.getPartiesList().stream())
                .filter(Objects::nonNull).map(Parties::getId).filter(Objects::nonNull).distinct().toList();
        List<Long> pickupDeliveryDetailsPartiesIds = ensureNonEmptyIds(pickupDeliveryDetailsPartiesIdsList);
        partiesDao.deleteAdditionalPartiesInPickupDeliveryDetailsByEntityIdAndEntityType(pickupDeliveryDetailsPartiesIds, pickupDeliveryDetailsIds, Constants.PICKUP_DELIVERY);
        pickupDeliveryDetailsDao.revertSoftDeleteByPickupDeliveryDetailsIdsAndShipmentId(pickupDeliveryDetailsIds, shipmentId);
        pickupDeliveryDetailsRepository.saveAll(shipmentDetails.getPickupDeliveryDetailsInstructions());
    }

    private void validateAndSetShipmentOrderDetails(Long shipmentId, List<Long> shipmentOrderIdsList, ShipmentDetails shipmentDetails) {
        List<Long> shipmentOrderIds = ensureNonEmptyIds(shipmentOrderIdsList);
        shipmentOrderDao.deleteAdditionalShipmentOrderByShipmentId(shipmentOrderIds, shipmentId);
        shipmentOrderDao.revertSoftDeleteByshipmentOrderIdsAndShipmentId(shipmentOrderIds, shipmentId);
        shipmentOrderRepository.saveAll(shipmentDetails.getShipmentOrders());
    }

    private void validateAndSetPartiesDetails(Long shipmentId, List<Long> partiesIdsList, ShipmentDetails shipmentDetails) {
        List<Long> partiesIds = ensureNonEmptyIds(partiesIdsList);
        partiesDao.deleteAdditionalDataByPartiesIdsEntityIdAndEntityType(partiesIds, shipmentId, Constants.SHIPMENT_ADDRESSES);
        partiesDao.revertSoftDeleteByPartiesIds(partiesIds);
        partiesRepository.saveAll(shipmentDetails.getShipmentAddresses());
    }

    private void validateAndSetJobsDetails(Long shipmentId, List<Long> jobsIdsList, ShipmentDetails shipmentDetails) {
        List<Long> jobsIds = ensureNonEmptyIds(jobsIdsList);
        iJobRepository.deleteAdditionalJobsByShipmentId(jobsIds, shipmentId);
        iJobRepository.revertSoftDeleteByJobsIdsAndShipmentId(jobsIds, shipmentId);
        iJobRepository.saveAll(shipmentDetails.getJobsList());
    }

    private void validateAndSetNotesDetails(Long shipmentId, List<Long> notesIdsList, ShipmentDetails shipmentDetails) {
        List<Long> notesIds = ensureNonEmptyIds(notesIdsList);
        notesDao.deleteAdditionalNotesByEntityIdAndEntityType(notesIds, shipmentId, Constants.SHIPMENT);
        notesDao.revertSoftDeleteByNotesIdsAndEntityIdAndEntityType(notesIds, shipmentId, Constants.SHIPMENT);
        notesRepository.saveAll(shipmentDetails.getNotesList());
    }

    private void validateAndSetTruckDriverDetails(Long shipmentId, List<Long> truckDriverDetailsIdsList, ShipmentDetails shipmentDetails) {
        List<Long> truckDriverDetailsIds = ensureNonEmptyIds(truckDriverDetailsIdsList);
        truckDriverDetailsDao.deleteAdditionalTruckDriverDetailsByShipmentId(truckDriverDetailsIds, shipmentId);
        truckDriverDetailsDao.revertSoftDeleteByTruckDriverDetailsIdsAndShipmentId(truckDriverDetailsIds, shipmentId);
        iTruckDriverDetailsRepository.saveAll(shipmentDetails.getTruckDriverDetails());
    }

    private void validateAndSetServiceDetails(Long shipmentId, List<Long> serviceDetailsIdsList, ShipmentDetails shipmentDetails) {
        List<Long> serviceDetailsIds = ensureNonEmptyIds(serviceDetailsIdsList);
        serviceDetailsDao.deleteAdditionalServiceDetailsByShipmentId(serviceDetailsIds, shipmentId);
        serviceDetailsDao.revertSoftDeleteByServiceDetailsIdsAndShipmentId(serviceDetailsIds, shipmentId);
        serviceDetailsRepository.saveAll(shipmentDetails.getServicesList());
    }

    private void validateAndSetRoutingsIds(Long shipmentId, List<Long> routingsIdsList, ShipmentDetails shipmentDetails) {
        List<Long> routingsIds = ensureNonEmptyIds(routingsIdsList);
        routingsDao.deleteAdditionalroutingsByShipmentId(routingsIds, shipmentId);
        routingsDao.revertSoftDeleteByroutingsIdsAndShipmentId(routingsIds, shipmentId);
        routingsRepository.saveAll(shipmentDetails.getRoutingsList());
    }

    private void validateAndSetReferenceNumbersDetails(Long shipmentId, List<Long> referenceNumbersIdsList, ShipmentDetails shipmentDetails) {
        List<Long> referenceNumbersIds = ensureNonEmptyIds(referenceNumbersIdsList);
        referenceNumbersDao.deleteAdditionalreferenceNumbersByShipmentId(referenceNumbersIds, shipmentId);
        referenceNumbersDao.revertSoftDeleteByreferenceNumbersIdsAndShipmentId(referenceNumbersIds, shipmentId);
        referenceNumbersRepository.saveAll(shipmentDetails.getReferenceNumbersList());
    }

    private void validateAndSetEventsDetails(Long shipmentId, List<Long> eventsIdsList, ShipmentDetails shipmentDetails) {
        List<Long> eventsIds = ensureNonEmptyIds(eventsIdsList);
        eventDao.deleteAdditionalEventDetailsByEntityIdAndEntityType(eventsIds, shipmentId, Constants.SHIPMENT);
        eventDao.revertSoftDeleteByEventDetailsIdsAndEntityIdAndEntityType(eventsIds, shipmentId, Constants.SHIPMENT);
        eventRepository.saveAll(shipmentDetails.getEventsList());
    }

    private void validateAndSetElDetails(Long shipmentId, List<Long> elDetailsIdsList, ShipmentDetails shipmentDetails) {
        List<Long> elDetailsIds = ensureNonEmptyIds(elDetailsIdsList);
        elDetailsDao.deleteAdditionalElDetailsByShipmentId(elDetailsIds, shipmentId);
        elDetailsDao.revertSoftDeleteByElDetailsIdsAndShipmentId(elDetailsIds, shipmentId);
        ielDetailsRepository.saveAll(shipmentDetails.getElDetailsList());
    }

    private void validateAndSetBookingCarriagesDetails(Long shipmentId, List<Long> bookingCarriageIdsList, ShipmentDetails shipmentDetails) {
        List<Long> bookingCarriageIds = ensureNonEmptyIds(bookingCarriageIdsList);
        bookingCarriageDao.deleteAdditionalbookingCarriageByShipmentId(bookingCarriageIds, shipmentId);
        bookingCarriageDao.revertSoftDeleteBybookingCarriageIdsAndShipmentId(bookingCarriageIds, shipmentId);
        bookingCarriageRepository.saveAll(shipmentDetails.getBookingCarriagesList());
    }

    private void validateAndSetPackingDetails(Long shipmentId, List<Long> packingIdsList, ShipmentDetails shipmentDetails) {
        List<Long> packingIds = ensureNonEmptyIds(packingIdsList);
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

        for (Long shipmentId : nonAttachedShipmentIds) {
            try {
                restoreShipmentTransaction(shipmentId, tenantId);
            } catch (Exception e) {
                log.error("Failed to restore Shipment id: {}", shipmentId, e);
                throw new RestoreFailureException("Failed to restore Shipment: " + shipmentId, e);
            }
        }
    }

    public void restoreShipmentTransaction(Long shipmentId, Integer tenantId) throws JsonProcessingException {
        TenantContext.setCurrentTenant(tenantId);
        UserContext.setUser(UsersDto.builder().Permissions(new HashMap<>()).build());
        restoreShipmentDetails(shipmentId, null, null);
        v1Service.clearAuthContext();
    }

    public static List<Long> ensureNonEmptyIds(List<Long> ids) {
        return (ids == null || ids.isEmpty()) ? List.of(-1L) : ids;
    }

    private void validateAndRestoreTriangularPartnerDetails(Long shipmentId) {
        shipmentDao.deleteTriangularPartnerShipmentByShipmentId(shipmentId);
    }
}
