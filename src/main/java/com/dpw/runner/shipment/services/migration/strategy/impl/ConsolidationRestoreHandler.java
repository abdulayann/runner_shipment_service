package com.dpw.runner.shipment.services.migration.strategy.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.impl.*;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.dpw.runner.shipment.services.migration.HelperExecutor;
import com.dpw.runner.shipment.services.migration.dao.impl.ConsolidationBackupDao;
import com.dpw.runner.shipment.services.migration.entity.ConsolidationBackupEntity;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.RestoreServiceHandler;
import com.dpw.runner.shipment.services.migration.utils.MigrationUtil;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IEventRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IJobRepository;
import com.dpw.runner.shipment.services.repository.interfaces.INetworkTransferRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IPartiesRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IReferenceNumbersRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IRoutingsRepository;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Generated;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.dpw.runner.shipment.services.migration.utils.MigrationUtil.futureCompletion;

@Service
@Slf4j
@RequiredArgsConstructor
@Generated
@SuppressWarnings({"java:S4144", "java:S1192"})
public class ConsolidationRestoreHandler implements RestoreServiceHandler {

    private final ObjectMapper objectMapper;
    private final ConsolidationBackupDao consolidationBackupDao;
    private final ConsolidationDao consolidationDao;
    private final ReferenceNumbersDao referenceNumbersDao;
    private final IReferenceNumbersRepository referenceNumbersRepository;
    private final RoutingsDao routingsDao;
    private final IRoutingsRepository routingsRepository;
    private final ContainerDao containerDao;
    private final IContainerRepository containerRepository;
    private final EventDao eventDao;
    private final IEventRepository eventRepository;
    private final PartiesDao partiesDao;
    private final IPartiesRepository partiesRepository;
    private final IJobRepository iJobRepository;
    private final ShipmentRestoreHandler shipmentRestoreHandler;
    private final ShipmentDao shipmentDao;
    private final V1ServiceImpl v1Service;
    @Autowired
    private HelperExecutor trxExecutor;

    @Autowired
    private NetworkTransferDao networkTransferDao;

    @Autowired
    private INetworkTransferRepository networkTransferRepository;

    @Autowired
    private MigrationUtil migrationUtil;


    @Override
    public void restore(Integer tenantId) {

        log.info("Starting consolidation restore for tenantId: {}", tenantId);

        List<ConsolidationBackupEntity> consolidationBackupEntities = consolidationBackupDao.findConsolidationIdsByTenantId(tenantId);
        Set<Long> consolidationIds = consolidationBackupEntities.stream().map(ConsolidationBackupEntity::getConsolidationId)
                .collect(Collectors.toSet());

        log.info("Total consolidation IDS : {}", consolidationIds.size());
        if (consolidationIds.isEmpty()) {
            log.info("No consolidation records found for tenant: {}", tenantId);
            return;
        }

        consolidationDao.deleteAdditionalConsolidationsByConsolidationIdAndTenantId(new ArrayList<>(consolidationIds), tenantId);

        consolidationIds = consolidationBackupEntities.stream().filter(ids -> !ids.getIsDeleted())
                .map(ConsolidationBackupEntity::getConsolidationId).collect(Collectors.toSet());
        consolidationDao.revertSoftDeleteByByConsolidationIdAndTenantId(new ArrayList<>(consolidationIds), tenantId);

        List<CompletableFuture<Object>> consolefutures = consolidationIds.stream()
                .map(id -> trxExecutor.runInAsyncForConsole(() -> {
                    try {
                        v1Service.setAuthContext();
                        TenantContext.setCurrentTenant(tenantId);
                        UserContext.getUser().setPermissions(new HashMap<>());
                        return trxExecutor.runInTrx(() -> {
                            processAndRestoreConsolidation(id, tenantId);
                            return null;
                        });
                    } catch (Exception e) {
                        log.error("Consolidation migration failed [id={}]: {}", id, e.getMessage(), e);
                        migrationUtil.saveErrorResponse(
                                id,
                                Constants.CONSOLIDATION,
                                IntegrationType.RESTORE_DATA_SYNC,
                                Status.FAILED,
                                e.getLocalizedMessage()
                        );
                        throw new IllegalArgumentException(e);
                    } finally {
                        v1Service.clearAuthContext();
                    }
                }))
                .toList();

        futureCompletion(consolefutures);
        log.info("Completed consolidation backup for tenant: {}", tenantId);
    }

    public void processAndRestoreConsolidation(Long consolidationId, Integer tenantId) {
        try {
            log.info("Started processing of consol id : {}", consolidationId);
            TenantContext.setCurrentTenant(tenantId);
            UserContext.setUser(UsersDto.builder().Permissions(new HashMap<>()).build());
            ConsolidationBackupEntity consolidationBackupDetails = consolidationBackupDao.findConsolidationsById(consolidationId);
            if (Objects.isNull(consolidationBackupDetails)) {
                log.info("No Consolidation records found for consol id : {}", consolidationId);
                return;
            }
            ConsolidationDetails consolidationDetails = objectMapper.readValue(consolidationBackupDetails.getConsolidationDetails(), ConsolidationDetails.class);
            Map<Long, List<Long>> containerShipmentMap = new HashMap<>();
            if (consolidationBackupDetails.getConsoleShipmentMapping() != null) {
                List<ConsoleShipmentMapping> mappingList = objectMapper.readValue(
                        consolidationBackupDetails.getConsoleShipmentMapping(),
                        new TypeReference<>() {
                        });
                List<Long> shipmentIds = mappingList.stream().map(ConsoleShipmentMapping::getShipmentId).filter(Objects::nonNull).toList();
                shipmentDao.revertSoftDeleteShipmentIdAndTenantId(shipmentIds, tenantId);
                revertContainers(consolidationDetails);
                for (Long shipmentId : shipmentIds) {
                    log.info("Started processing of shipment id : {} and console id :{}", shipmentId, consolidationId);
                    if (consolidationDetails.getShipmentsList() == null) {
                        consolidationDetails.setShipmentsList(new HashSet<>());
                    }
                    ShipmentDetails shipmentDetails = shipmentRestoreHandler.restoreShipmentDetails(shipmentId, containerShipmentMap, consolidationDetails);
                    if (shipmentDetails != null) {
                        consolidationDetails.getShipmentsList().add(shipmentDetails);
                    }
                    log.info("Completed processing of shipment id : {} and console id :{}", shipmentId, consolidationId);
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
            List<NetworkTransfer> networkTransferList = consolidationBackupDetails.getNetworkTransferDetails() != null && !consolidationBackupDetails.getNetworkTransferDetails().isEmpty() ? objectMapper.readValue(consolidationBackupDetails.getNetworkTransferDetails(), new TypeReference<List<NetworkTransfer>>() {
            }) : new ArrayList<>();
            validateAndSetNetworkTransferDetails(networkTransferList, consolidationId);
            validateAndRestoreTriangularPartnerDetails(consolidationId);
            consolidationDao.save(consolidationDetails);
            consolidationBackupDao.makeIsDeleteTrueToMarkRestoreSuccessful(consolidationBackupDetails.getId());
            log.info("Completed processing of consol id : {}", consolidationId);
        } catch (Exception e) {
            log.error("Failed to backup consolidation id: {} with exception: ", consolidationId, e);
            throw new IllegalArgumentException(e);
        } finally {
            v1Service.clearAuthContext();
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
            log.info("Started processing of network transfer id : {} and console id :{}", incoming.getId(), consolidationId);

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

             incoming.setId(null);
            toSaveList.add(incoming);
            log.info("Completed processing of network transfer id : {} and console id :{}", incoming.getId(), consolidationId);
        }

        List<Long> toDeleteIds = new ArrayList<>(dbMap.keySet());

        networkTransferRepository.saveAll(toSaveList);
        networkTransferRepository.deleteAllById(toDeleteIds);
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
        List<Long> containersIdsList = consolidationDetails.getContainersList().stream().map(Containers::getId).filter(Objects::nonNull).toList();
        List<Long> containersIds = ensureNonEmptyIds(containersIdsList);
        containerDao.deleteAdditionalDataByContainersIdsConsolidationId(containersIds, consolidationDetails.getId());
        containerDao.revertSoftDeleteByContainersIdsAndConsolidationId(containersIds, consolidationDetails.getId());
    }

    private void validateAndSaveContainers(ConsolidationDetails consolidationDetails, Map<Long, List<Long>> containerShipmentMap) {
        List<Containers> containers = consolidationDetails.getContainersList();

        List<ShipmentDetails> shipmentDetailsList = nullSafeCollectionStream(consolidationDetails.getShipmentsList()).toList();
        if (!shipmentDetailsList.isEmpty()) {
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
        }
        containerRepository.saveAll(containers);
    }

    private void validateAndStoreJobsDetails(Long consolidationId, List<Long> jobsIdsList, ConsolidationDetails consolidationDetails) {
        List<Long> jobsIds = ensureNonEmptyIds(jobsIdsList);
        iJobRepository.deleteAdditionalDataByJobsIdsAndConsolidationId(jobsIds, consolidationId);
        iJobRepository.revertSoftDeleteByJobsIdsAndConsolidationId(jobsIds, consolidationId);
        iJobRepository.saveAll(consolidationDetails.getJobsList());
    }

    private void validateAndRestorePartiesDetails(Long consolidationId, List<Long> consolidationAddressIdsList, ConsolidationDetails consolidationDetails) {
        List<Long> consolidationAddressIds = ensureNonEmptyIds(consolidationAddressIdsList);
        partiesDao.deleteAdditionalDataByPartiesIdsEntityIdAndEntityType(consolidationAddressIds, consolidationId, Constants.CONSOLIDATION_ADDRESSES);
        partiesDao.revertSoftDeleteByPartiesIds(consolidationAddressIds);
        partiesRepository.saveAll(consolidationDetails.getConsolidationAddresses());
    }

    private void validateAndRestoreEventsDetails(Long consolidationId, List<Long> eventsIdsList, ConsolidationDetails consolidationDetails) {
        List<Long> eventsIds = ensureNonEmptyIds(eventsIdsList);
        eventDao.deleteAdditionalDataByEventsIdsConsolidationId(eventsIds, consolidationId);
        eventDao.revertSoftDeleteByEventsIds(eventsIds);
        eventRepository.saveAll(consolidationDetails.getEventsList());
    }

    private void validateAndRestoreRoutingDetails(Long consolidationId, List<Long> routingsIdsList, ConsolidationDetails consolidationDetails) {
        List<Long> routingsIds = ensureNonEmptyIds(routingsIdsList);
        routingsDao.deleteAdditionalDataByRoutingsIdsConsolidationId(routingsIds, consolidationId);
        routingsDao.revertSoftDeleteByRoutingsIdsAndConsolidationId(routingsIds, consolidationId);
        routingsRepository.saveAll(consolidationDetails.getRoutingsList());
    }

    private void validateAndRestoreReferenceNumberDetails(Long consolidationId, List<Long> referenceNumberIdsList, ConsolidationDetails consolidationDetails) {
        List<Long> referenceNumberIds = ensureNonEmptyIds(referenceNumberIdsList);
        referenceNumbersDao.deleteAdditionalDataByReferenceNumberIdsConsolidationId(referenceNumberIds, consolidationId);
        referenceNumbersDao.revertSoftDeleteByReferenceNumberIdsAndConsolidationId(referenceNumberIds, consolidationId);
        referenceNumbersRepository.saveAll(consolidationDetails.getReferenceNumbersList());
    }

    public static List<Long> ensureNonEmptyIds(List<Long> ids) {
        return (ids == null || ids.isEmpty()) ? List.of(-1L) : ids;
    }

    private void validateAndRestoreTriangularPartnerDetails(Long consolidationId) {
        consolidationDao.deleteTriangularPartnerConsolidationByConsolidationId(consolidationId);
    }
}
