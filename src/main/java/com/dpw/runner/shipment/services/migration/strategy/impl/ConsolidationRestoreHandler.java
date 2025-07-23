package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.impl.*;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.dao.impl.ConsolidationBackupDao;
import com.dpw.runner.shipment.services.migration.entity.ConsolidationBackupEntity;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.RestoreHandler;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IEventRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IFileRepoRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IJobRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPackingRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPartiesRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IReferenceNumbersRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IRoutingsRepository;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
@Slf4j
public class ConsolidationRestoreHandler implements RestoreHandler {

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private ThreadPoolTaskExecutor asyncExecutor;

    @Autowired
    @Lazy
    private ConsolidationRestoreHandler self;

    @Autowired
    private ConsolidationBackupDao consolidationBackupDao;

    @Autowired
    private ConsolidationDao consolidationDao;

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
    private IContainerRepository containerRepository;

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
    private ShipmentRestoreHandler shipmentRestoreHandler;

    @Autowired
    private ShipmentDao shipmentDao;

    @Autowired
    private IFileRepoRepository iFileRepoRepository;

    @Autowired
    private NetworkTransferDao networkTransferDao;

    @Override
    public void restore(Integer tenantId) {

        log.info("Starting consolidation restore for tenantId: {}", tenantId);

        List<Long> consolidationIds = consolidationBackupDao.findConsolidationIdsByTenantId(tenantId);
        if (consolidationIds.isEmpty()) {
            log.info("No consolidation records found for tenant: {}", tenantId);
            return;
        }
        consolidationDao.deleteAdditionalConsolidationsByConsolidationIdAndTenantId(consolidationIds, tenantId);
        consolidationDao.revertSoftDeleteByByConsolidationIdAndTenantId(consolidationIds, tenantId);


        List<CompletableFuture<Void>> futures = consolidationIds.stream()
                .map(consolidationId -> CompletableFuture.runAsync(
                        () -> {
                            try {
                                self.processAndRestoreConsolidation(consolidationId, tenantId);
                            } finally {
                                TenantContext.removeTenant();
                                UserContext.removeUser();
                            }
                        },
                        asyncExecutor))
                .toList();

        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        log.info("Completed consolidation backup for tenant: {}", tenantId);
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void processAndRestoreConsolidation(Long consolidationId, Integer tenantId) {
        try {
            TenantContext.setCurrentTenant(tenantId);
            UserContext.setUser(UsersDto.builder().Permissions(new HashMap<>()).build());
            ConsolidationBackupEntity consolidationBackupDetails = consolidationBackupDao.findConsolidationsById(consolidationId);
            ConsolidationDetails consolidationDetails = objectMapper.readValue(consolidationBackupDetails.getConsolidationDetails(), ConsolidationDetails.class);
            Map<Long, List<Long>> containerShipmentMap = new HashMap<>();
            if (consolidationBackupDetails.getConsoleShipmentMapping() != null) {
                List<ConsoleShipmentMapping> mappingList = objectMapper.readValue(
                        consolidationBackupDetails.getConsoleShipmentMapping(),
                        new TypeReference<>() {
                        }
                );
                List<Long> shipmentIds = mappingList.stream().map(ConsoleShipmentMapping::getShipmentId).filter(Objects::nonNull).toList();
                shipmentDao.revertSoftDeleteShipmentIdAndTenantId(shipmentIds, tenantId);
                revertContainers(consolidationDetails);
                for (Long shipmentId : shipmentIds) {
                    if (consolidationDetails.getShipmentsList() == null) {
                        consolidationDetails.setShipmentsList(new HashSet<>());
                    }
                    ShipmentDetails shipmentDetails = shipmentRestoreHandler.restoreShipmentDetails(shipmentId, containerShipmentMap, consolidationDetails);
                    if (shipmentDetails != null) {
                        consolidationDetails.getShipmentsList().add(shipmentDetails);
                    }
                }
            }
            List<Long> allPartiesIds = getAllPartiesIds(consolidationDetails);
            validateAndRestorePartiesDetails(consolidationId, allPartiesIds, consolidationDetails);
            List<Long> eventsIds = getAllEventsIds(consolidationDetails);
            validateAndRestoreEventsDetails(consolidationId, eventsIds, consolidationDetails);
            validateAndSaveContainers(consolidationDetails, containerShipmentMap);
            List<Long> referenceNumberIds = consolidationDetails.getReferenceNumbersList().stream().map(ReferenceNumbers::getId).filter(Objects::nonNull).toList();
            validateAndRestoreReferenceNumberDetails(consolidationId, referenceNumberIds, consolidationDetails);
            List<Long> routingsIds = consolidationDetails.getRoutingsList().stream().map(Routings::getId).filter(Objects::nonNull).toList();
            validateAndRestoreRoutingDetails(consolidationId, routingsIds, consolidationDetails);
            List<Long> jobsIds = consolidationDetails.getJobsList().stream().map(Jobs::getId).filter(Objects::nonNull).toList();
            validateAndStoreJobsDetails(consolidationId, jobsIds, consolidationDetails);
            List<NetworkTransfer> networkTransferList = consolidationBackupDetails.getNetworkTransferDetails()!=null && !consolidationBackupDetails.getNetworkTransferDetails().isEmpty() ? objectMapper.readValue(consolidationBackupDetails.getNetworkTransferDetails(), new TypeReference<List<NetworkTransfer>>() {}): new ArrayList<>();
            validateAndSetNetworkTransferDetails(networkTransferList, consolidationId);
//TODO packking to containersIds

            consolidationDao.save(consolidationDetails);
            consolidationBackupDao.makeIsDeleteTrueToMarkRestoreSuccessful(consolidationBackupDetails.getId());
        } catch (Exception e) {
            log.error("Failed to backup consolidation id: {} with exception: ", consolidationId, e);
            throw new BackupFailureException("Failed to backup consolidation id: " + consolidationId, e);
        }
    }

    private void validateAndSetNetworkTransferDetails(List<NetworkTransfer> networkTransferList, Long consolidationId) {
        List<NetworkTransfer> networkTransferDbList = networkTransferDao.findByEntityNTList(consolidationId, Constants.CONSOLIDATION_ID);

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
        networkTransferDao.saveAll(toSaveList);
        networkTransferDao.deleteByIdsAndLog(toDeleteIds);
    }

    private List<Long> getAllEventsIds(ConsolidationDetails consolidationDetails) {
        return Stream.of(
                nullSafeCollectionStream(consolidationDetails.getJobsList()).flatMap(job -> nullSafeCollectionStream(job.getEventsList())),
                nullSafeCollectionStream(consolidationDetails.getContainersList()).flatMap(container -> nullSafeCollectionStream(container.getEventsList())),
                nullSafeCollectionStream(consolidationDetails.getEventsList())).flatMap(Function.identity()).map(Events::getId).filter(Objects::nonNull).distinct().toList();
    }

    private static <T> Stream<T> nullSafeCollectionStream(Collection<T> collection) {
        return (collection == null) ? Stream.empty() : collection.stream();
    }

    private static List<Long> getAllPartiesIds(ConsolidationDetails consolidationDetails) {
        return Stream.of(
                nullSafeCollectionStream(consolidationDetails.getContainersList()).flatMap(container -> Stream.of(container.getPickupAddress(), container.getDeliveryAddress())),
                nullSafeCollectionStream(consolidationDetails.getJobsList()).flatMap(job -> Stream.of(job.getBuyerDetail(), job.getSupplierDetail())),
                nullSafeCollectionStream(consolidationDetails.getConsolidationAddresses())).flatMap(Function.identity()).filter(Objects::nonNull).map(Parties::getId).filter(Objects::nonNull).distinct().toList();
    }

    private void revertContainers(ConsolidationDetails consolidationDetails) {
        List<Long> containersIds = consolidationDetails.getContainersList().stream().map(Containers::getId).filter(Objects::nonNull).toList();
        containerDao.deleteAdditionalDataByContainersIdsConsolidationId(containersIds, consolidationDetails.getId());
        containerDao.revertSoftDeleteByContainersIdsAndConsolidationId(containersIds, consolidationDetails.getId());
    }

    private void validateAndSaveFileRepoDetails(Long consolidationId, List<Long> fileRepoIds, ConsolidationDetails consolidationDetails) {
        iFileRepoRepository.deleteAdditionalDataByFileRepoIdsEntityIdAndEntityType(fileRepoIds, consolidationId, Constants.CONSOLIDATION);
        iFileRepoRepository.revertSoftDeleteByFileRepoIdsEntityIdAndEntityType(fileRepoIds, consolidationId, Constants.CONSOLIDATION);
        for (FileRepo restored : consolidationDetails.getFileRepoList()) {
            iFileRepoRepository.save(restored);
        }
    }

    private void validateAndSaveContainers(ConsolidationDetails consolidationDetails, Map<Long, List<Long>> containerShipmentMap) {
        List<Containers> containers = consolidationDetails.getContainersList();

        List<ShipmentDetails> shipmentDetailsList = consolidationDetails.getShipmentsList().stream().toList();
        Map<Long, ShipmentDetails> shipmentDetailsMap = shipmentDetailsList.stream()
                .collect(Collectors.toMap(ShipmentDetails::getId, Function.identity()));
        for (Containers container : containers) {
            List<Long> shipmentsIds = containerShipmentMap.get(container.getId());
            if (shipmentsIds != null) {
                Set<ShipmentDetails> shipmentSet = (!shipmentsIds.isEmpty())
                        ? shipmentsIds.stream()
                        .map(shipmentDetailsMap::get)
                        .filter(Objects::nonNull)
                        .collect(Collectors.toSet())
                        : Collections.emptySet();
                container.setShipmentsList(shipmentSet);
            }
        }
        containerRepository.saveAll(containers);
    }

    private void validateAndStoreJobsDetails(Long consolidationId, List<Long> jobsIds, ConsolidationDetails consolidationDetails) {
        iJobRepository.deleteAdditionalDataByJobsIdsAndConsolidationId(jobsIds, consolidationId);
        iJobRepository.revertSoftDeleteByJobsIdsAndConsolidationId(jobsIds, consolidationId);
        iJobRepository.saveAll(consolidationDetails.getJobsList());
    }

    private void validateAndRestorePartiesDetails(Long consolidationId, List<Long> consolidationAddressIds, ConsolidationDetails consolidationDetails) {
        partiesDao.deleteAdditionalDataByPartiesIdsEntityIdAndEntityType(consolidationAddressIds, consolidationId, Constants.CONSOLIDATION_ADDRESSES);
        partiesDao.revertSoftDeleteByPartiesIds(consolidationAddressIds);
        partiesRepository.saveAll(consolidationDetails.getConsolidationAddresses());
    }

    private void validateAndRestoreEventsDetails(Long consolidationId, List<Long> eventsIds, ConsolidationDetails consolidationDetails) {
        eventDao.deleteAdditionalDataByEventsIdsConsolidationId(eventsIds, consolidationId);
        eventDao.revertSoftDeleteByEventsIds(eventsIds);
        eventRepository.saveAll(consolidationDetails.getEventsList());
    }

/*    private void validateAndRestoreContainersDetails(Long consolidationId, List<Long> containersIds, ConsolidationDetails consolidationDetails) {
        containerDao.deleteAdditionalDataByContainersIdsConsolidationId(containersIds, consolidationId);
        containerDao.revertSoftDeleteByContainersIdsAndConsolidationId(containersIds, consolidationId);
        for (Containers restored : consolidationDetails.getContainersList()) {
            containerDao.save(restored);
        }
    }*/

    private void validateAndRestoreRoutingDetails(Long consolidationId, List<Long> routingsIds, ConsolidationDetails consolidationDetails) {
        routingsDao.deleteAdditionalDataByRoutingsIdsConsolidationId(routingsIds, consolidationId);
        routingsDao.revertSoftDeleteByRoutingsIdsAndConsolidationId(routingsIds, consolidationId);
        routingsRepository.saveAll(consolidationDetails.getRoutingsList());
    }

    private void validateAndRestoreReferenceNumberDetails(Long consolidationId, List<Long> referenceNumberIds, ConsolidationDetails consolidationDetails) {
        referenceNumbersDao.deleteAdditionalDataByReferenceNumberIdsConsolidationId(referenceNumberIds, consolidationId);
        referenceNumbersDao.revertSoftDeleteByReferenceNumberIdsAndConsolidationId(referenceNumberIds, consolidationId);
        referenceNumbersRepository.saveAll(consolidationDetails.getReferenceNumbersList());
    }

    private void validateAndRestorePackingDetails(Long consolidationId, List<Long> packingIds, ConsolidationDetails consolidationDetails) {
        packingDao.deleteAdditionalPackingByConsolidationId(packingIds, consolidationId);
        packingDao.revertSoftDeleteByPackingIdsAndConsolidationId(packingIds, consolidationId);
        packingRepository.saveAll(consolidationDetails.getPackingList());
    }


}
