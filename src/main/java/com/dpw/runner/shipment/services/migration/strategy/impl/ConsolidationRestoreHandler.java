package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.impl.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.BackupFailureException;
import com.dpw.runner.shipment.services.migration.dao.impl.ConsolidationBackupDao;
import com.dpw.runner.shipment.services.migration.entity.ConsolidationBackupEntity;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.RestoreHandler;
import com.dpw.runner.shipment.services.repository.interfaces.IFileRepoRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IJobRepository;
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
    private ShipmentRestoreHandler shipmentRestoreHandler;

    @Autowired
    private ShipmentDao shipmentDao;

    @Autowired
    private IFileRepoRepository iFileRepoRepository;

    @Override
    public void restore(Integer tenantId) {

        log.info("Starting consolidation restore for tenantId: {}", tenantId);

        // for local test
/*            Long consolidationId = 6990L;
            ConsolidationBackupEntity consolidationBackupDetails = consolidationBackupDao.findConsolidationsById(consolidationId);
        try {
            ConsolidationDetails consolidationDetails = objectMapper.readValue(consolidationBackupDetails.getConsolidationDetails(), ConsolidationDetails.class);
            if (consolidationBackupDetails.getConsoleShipmentMapping() != null) {
                List<ConsoleShipmentMapping> mappingList = objectMapper.readValue(
                        consolidationBackupDetails.getConsoleShipmentMapping(),
                        new TypeReference<>() {
                        }
                );
                List<Long> shipmentIds = mappingList.stream().map(ConsoleShipmentMapping::getShipmentId).filter(Objects::nonNull).toList();
                shipmentDao.revertSoftDeleteShipmentIdAndTenantId(shipmentIds, tenantId);
                //shipmentDao
                //TODO need to delete the new set of shipment data.

                Map<Long, List<Long>> containerShipmentMap = new HashMap<>();
                for (Long shipmentId : shipmentIds) {
                    shipmentRestoreHandler.restoreShipmentDetails(shipmentId, containerShipmentMap);
                }

                // add this in to another class
                List<Long> containersIds = consolidationDetails.getContainersList().stream().map(Containers::getId).filter(Objects::nonNull).toList();
                containerDao.deleteAdditionalDataByContainersIdsConsolidationId(containersIds, consolidationId);
                containerDao.revertSoftDeleteByContainersIdsAndConsolidationId(containersIds, consolidationId);
                List<Containers> containers = containerDao.findByIdIn(containersIds);
                List<Long> allShipmentIdsFromContainerMap = containerShipmentMap.values().stream().flatMap(List::stream).distinct().toList();
//                shipmentDao.revertSoftDeleteShipmentIdAndTenantId(allShipmentIdsFromContainerMap, tenantId);
                List<ShipmentDetails> shipmentDetailsList = shipmentDao.findByIdIn(allShipmentIdsFromContainerMap);
                Map<Long, ShipmentDetails> shipmentDetailsMap = shipmentDetailsList.stream()
                        .collect(Collectors.toMap(ShipmentDetails::getId, Function.identity()));
                for (Containers container : containers) {
                    List<Long> shipmentsIds = containerShipmentMap.get(container.getId());
                    if (shipmentsIds != null) {
                        Set<ShipmentDetails> shipmentSet = (!shipmentIds.isEmpty())
                                ? shipmentIds.stream()
                                .map(shipmentDetailsMap::get)
                                .filter(Objects::nonNull)
                                .collect(Collectors.toSet())
                                : Collections.emptySet();
                        container.setShipmentsList(shipmentSet);
                    }
                }
                containerDao.saveAll(containers);
                System.out.println("hi");
            }
        } catch (Exception e) {
            log.error("Failed to backup consolidation id: {} with exception: ", consolidationId, e);
        }*/

        List<Long> consolidationIds = consolidationBackupDao.findConsolidationIdsByTenantId(tenantId);
        consolidationDao.deleteAdditionalConsolidationsByConsolidationIdAndTenantId(consolidationIds, tenantId);
        consolidationDao.revertSoftDeleteByByConsolidationIdAndTenantId(consolidationIds, tenantId);

        if (consolidationIds.isEmpty()) {
            log.info("No consolidation records found for tenant: {}", tenantId);
            return;
        }

        List<CompletableFuture<Void>> futures = consolidationIds.stream()
                .map(consolidationId -> CompletableFuture.runAsync(
                        () -> self.processAndRestoreConsolidation(consolidationId),
                        asyncExecutor))
                .toList();

        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        log.info("Completed consolidation backup for tenant: {}", tenantId);
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void processAndRestoreConsolidation(Long consolidationId) {
        try {
            ConsolidationBackupEntity consolidationBackupDetails = consolidationBackupDao.findConsolidationsById(consolidationId);
            Integer tenantId = consolidationBackupDetails.getTenantId();
            ConsolidationDetails consolidationDetails = objectMapper.readValue(consolidationBackupDetails.getConsolidationDetails(), ConsolidationDetails.class);
            if (consolidationBackupDetails.getConsoleShipmentMapping() != null) {
                List<ConsoleShipmentMapping> mappingList = objectMapper.readValue(
                        consolidationBackupDetails.getConsoleShipmentMapping(),
                        new TypeReference<>() {
                        }
                );
                List<Long> shipmentIds = mappingList.stream().map(ConsoleShipmentMapping::getShipmentId).filter(Objects::nonNull).toList();
                shipmentDao.revertSoftDeleteShipmentIdAndTenantId(shipmentIds, tenantId);
                //TODO need to delete the new set of shipment data.
                //TODO need to diss for flag

                Map<Long, List<Long>> containerShipmentMap = new HashMap<>();
                for (Long shipmentId : shipmentIds) {
                    shipmentRestoreHandler.restoreShipmentDetails(shipmentId, containerShipmentMap);
                }
                validateAndSaveContainers(consolidationId, consolidationDetails, containerShipmentMap, shipmentIds);
            }
            List<Long> packingIds = consolidationDetails.getPackingList().stream().map(Packing::getId).filter(Objects::nonNull).toList();
            validateAndRestorePackingDetails(consolidationId, packingIds, consolidationDetails);
            List<Long> referenceNumberIds = consolidationDetails.getReferenceNumbersList().stream().map(ReferenceNumbers::getId).filter(Objects::nonNull).toList();
            validateAndRestoreReferenceNumberDetails(consolidationId, referenceNumberIds, consolidationDetails);
            List<Long> routingsIds = consolidationDetails.getRoutingsList().stream().map(Routings::getId).filter(Objects::nonNull).toList();
            validateAndRestoreRoutingDetails(consolidationId, routingsIds, consolidationDetails);
            List<Long> eventsIds = consolidationDetails.getEventsList().stream().map(Events::getId).filter(Objects::nonNull).toList();
            validateAndRestoreEventsDetails(consolidationId, eventsIds, consolidationDetails);
            List<Long> consolidationAddressIds = consolidationDetails.getConsolidationAddresses().stream().map(Parties::getId).filter(Objects::nonNull).toList();
            validateAndRestorePartiesDetails(consolidationId, consolidationAddressIds, consolidationDetails);
            List<Long> jobsIds = consolidationDetails.getJobsList().stream().map(Jobs::getId).filter(Objects::nonNull).toList();
            validateAndStoreJobsDetails(consolidationId, jobsIds, consolidationDetails);
            List<Long> fileRepoIds = consolidationDetails.getFileRepoList().stream().map(FileRepo::getId).filter(Objects::nonNull).toList();
            validateAndSaveFileRepoDetails(consolidationId, fileRepoIds, consolidationDetails);
//TODO packking to containersIds

            consolidationDao.save(consolidationDetails);
        } catch (Exception e) {
            log.error("Failed to backup consolidation id: {} with exception: ", consolidationId, e);
            throw new BackupFailureException("Failed to backup consolidation id: " + consolidationId, e);
        }
    }

    private void validateAndSaveFileRepoDetails(Long consolidationId, List<Long> fileRepoIds, ConsolidationDetails consolidationDetails) {
        iFileRepoRepository.deleteAdditionalDataByFileRepoIdsEntityIdAndEntityType(fileRepoIds, consolidationId, Constants.CONSOLIDATION);
        iFileRepoRepository.revertSoftDeleteByFileRepoIdsEntityIdAndEntityType(fileRepoIds, consolidationId, Constants.CONSOLIDATION);
        for (FileRepo restored : consolidationDetails.getFileRepoList()) {
            iFileRepoRepository.save(restored);
        }
    }

    private void validateAndSaveContainers(Long consolidationId, ConsolidationDetails consolidationDetails, Map<Long, List<Long>> containerShipmentMap, List<Long> shipmentIds) {
        List<Long> containersIds = consolidationDetails.getContainersList().stream().map(Containers::getId).filter(Objects::nonNull).toList();
        containerDao.deleteAdditionalDataByContainersIdsConsolidationId(containersIds, consolidationId);
        containerDao.revertSoftDeleteByContainersIdsAndConsolidationId(containersIds, consolidationId);
        List<Containers> containers = containerDao.findByIdIn(containersIds);
        List<Long> allShipmentIdsFromContainerMap = containerShipmentMap.values().stream().flatMap(List::stream).distinct().toList();
        List<ShipmentDetails> shipmentDetailsList = shipmentDao.findByIdIn(allShipmentIdsFromContainerMap);
        Map<Long, ShipmentDetails> shipmentDetailsMap = shipmentDetailsList.stream()
                .collect(Collectors.toMap(ShipmentDetails::getId, Function.identity()));
        for (Containers container : containers) {
            List<Long> shipmentsIds = containerShipmentMap.get(container.getId());
            if (shipmentsIds != null) {
                Set<ShipmentDetails> shipmentSet = (!shipmentIds.isEmpty())
                        ? shipmentIds.stream()
                        .map(shipmentDetailsMap::get)
                        .filter(Objects::nonNull)
                        .collect(Collectors.toSet())
                        : Collections.emptySet();
                container.setShipmentsList(shipmentSet);
            }
        }
        containerDao.saveAll(containers);
    }

    private void validateAndStoreJobsDetails(Long consolidationId, List<Long> jobsIds, ConsolidationDetails consolidationDetails) {
        iJobRepository.deleteAdditionalDataByJobsIdsAndConsolidationId(jobsIds, consolidationId);
        iJobRepository.revertSoftDeleteByJobsIdsAndConsolidationId(jobsIds, consolidationId);
        for (Jobs restored : consolidationDetails.getJobsList()) {
            iJobRepository.save(restored);
        }
    }

    private void validateAndRestorePartiesDetails(Long consolidationId, List<Long> consolidationAddressIds, ConsolidationDetails consolidationDetails) {
        partiesDao.deleteAdditionalDataByPartiesIdsEntityIdAndEntityType(consolidationAddressIds, consolidationId, Constants.CONSOLIDATION_ADDRESSES);
        partiesDao.revertSoftDeleteByPartiesIdsEntityIdAndEntityType(consolidationAddressIds, consolidationId, Constants.CONSOLIDATION_ADDRESSES);
        for (Parties restored : consolidationDetails.getConsolidationAddresses()) {
            partiesDao.save(restored);
        }
    }

    private void validateAndRestoreEventsDetails(Long consolidationId, List<Long> eventsIds, ConsolidationDetails consolidationDetails) {
        eventDao.deleteAdditionalDataByEventsIdsConsolidationId(eventsIds, consolidationId);
        eventDao.revertSoftDeleteByEventsIdsAndConsolidationId(eventsIds, consolidationId);
        for (Events restored : consolidationDetails.getEventsList()) {
            eventDao.save(restored);
        }
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
        for (Routings restored : consolidationDetails.getRoutingsList()) {
            routingsDao.save(restored);
        }
    }

    private void validateAndRestoreReferenceNumberDetails(Long consolidationId, List<Long> referenceNumberIds, ConsolidationDetails consolidationDetails) {
        referenceNumbersDao.deleteAdditionalDataByReferenceNumberIdsConsolidationId(referenceNumberIds, consolidationId);
        referenceNumbersDao.revertSoftDeleteByReferenceNumberIdsAndConsolidationId(referenceNumberIds, consolidationId);
        for (ReferenceNumbers restored : consolidationDetails.getReferenceNumbersList()) {
            referenceNumbersDao.save(restored);
        }
    }

    private void validateAndRestorePackingDetails(Long consolidationId, List<Long> packingIds, ConsolidationDetails consolidationDetails) {
        packingDao.deleteAdditionalPackingByConsolidationId(packingIds, consolidationId);
        packingDao.revertSoftDeleteByPackingIdsAndConsolidationId(packingIds, consolidationId);
        for (Packing restored : consolidationDetails.getPackingList()) {
            packingDao.save(restored);
        }
    }


}
