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
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.annotations.Where;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

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
    private IFileRepoRepository iFileRepoRepository;

    @Override
    public void restore(Integer tenantId) {

        log.info("Starting consolidation restore for tenantId: {}", tenantId);
        List<Long> consolidationIds = consolidationBackupDao.findConsolidationIdsByTenantId(tenantId);
        //TODO need to delete the new set of consolidation data.

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

    //one to one is working even after removing the shipment details list
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void processAndRestoreConsolidation(Long consolidationId) {
        try {
            ConsolidationBackupEntity consolidationBackupDetails = consolidationBackupDao.findConsolidationsById(consolidationId);
            ConsolidationDetails consolidationDetails = objectMapper.readValue(consolidationBackupDetails.getConsolidationDetails(), ConsolidationDetails.class);
            List<Long> packingIds = consolidationDetails.getPackingList().stream().map(Packing::getId).filter(Objects::nonNull).toList();
            validateAndRestorePackingDetails(consolidationId, packingIds, consolidationDetails);// TODO it has consolidation and shipment
            List<Long> referenceNumberIds = consolidationDetails.getReferenceNumbersList().stream().map(ReferenceNumbers::getId).filter(Objects::nonNull).toList();
            validateAndRestoreReferenceNumberDetails(consolidationId, referenceNumberIds, consolidationDetails);
            List<Long> routingsIds = consolidationDetails.getRoutingsList().stream().map(Routings::getId).filter(Objects::nonNull).toList();
            validateAndRestoreRoutingDetails(consolidationId, routingsIds, consolidationDetails);
            List<Long> containersIds = consolidationDetails.getContainersList().stream().map(Containers::getId).filter(Objects::nonNull).toList();
            validateAndRestoreContainersDetails(consolidationId, containersIds, consolidationDetails);
            List<Long> eventsIds = consolidationDetails.getEventsList().stream().map(Events::getId).filter(Objects::nonNull).toList();
            validateAndRestoreEventsDetails(consolidationId, eventsIds, consolidationDetails);// TODO need to add entity id and type
            List<Long> consolidationAddressIds = consolidationDetails.getConsolidationAddresses().stream().map(Parties::getId).filter(Objects::nonNull).toList();
            validateAndRestorePartiesDetails(consolidationId, consolidationAddressIds, consolidationDetails);
            List<Long> jobsIds = consolidationDetails.getJobsList().stream().map(Jobs::getId).filter(Objects::nonNull).toList();
            validateAndStoreJobsDetails(consolidationId, jobsIds, consolidationDetails);
            // TODO need to discuss for entity type
            //List<Long> fileRepoIds = consolidationDetails.getFileRepoList().stream().map(FileRepo::getId).filter(Objects::nonNull).toList();
/*            iFileRepoRepository.deleteAdditionalDataByFileRepoIdsAndConsolidationId(fileRepoIds, consolidationId);
            iFileRepoRepository.revertSoftDeleteByFileRepoIdsAndConsolidationId(fileRepoIds, consolidationId);
            for (FileRepo restored : consolidationDetails.getFileRepoList()) {
                iFileRepoRepository.save(restored);
            }*/


            //TODO delete additional details
            //TODO mark is deleted false




//TODO packking to containersIds
//TODO shipment to container


            consolidationDao.save(consolidationDetails);// for one to one
        } catch (Exception e) {
            log.error("Failed to backup consolidation id: {} with exception: ", consolidationId, e);
            throw new BackupFailureException("Failed to backup consolidation id: " + consolidationId, e);
        }
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

    private void validateAndRestoreContainersDetails(Long consolidationId, List<Long> containersIds, ConsolidationDetails consolidationDetails) {
        containerDao.deleteAdditionalDataByContainersIdsConsolidationId(containersIds, consolidationId);
        containerDao.revertSoftDeleteByContainersIdsAndConsolidationId(containersIds, consolidationId);
        for (Containers restored : consolidationDetails.getContainersList()) {
            containerDao.save(restored);
        }
    }

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
