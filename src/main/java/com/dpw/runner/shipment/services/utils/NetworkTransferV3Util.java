package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.ICommonErrorLogsDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dao.interfaces.IQuartzJobInfoDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.JobState;
import com.dpw.runner.shipment.services.entity.enums.JobType;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.service.interfaces.INetworkTransferService;
import com.dpw.runner.shipment.services.service.interfaces.IQuartzJobInfoService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;

@Component
@Slf4j
public class NetworkTransferV3Util {

    @Autowired
    private IQuartzJobInfoService quartzJobInfoService;

    @Autowired
    private INetworkTransferDao networkTransferDao;

    @Autowired
    private IQuartzJobInfoDao quartzJobInfoDao;

    @Autowired
    private ICommonErrorLogsDao commonErrorLogsDao;

    @Autowired
    private INetworkTransferService networkTransferService;

    @Autowired
    private CommonUtils commonUtils;


    public void createOrUpdateNetworkTransferEntity(ShipmentSettingsDetails shipmentSettingsDetails, ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        try{
            boolean isNetworkTransferEntityEnabled = Boolean.TRUE.equals(shipmentSettingsDetails.getIsNetworkTransferEntityEnabled());
            if(consolidationDetails.getShipmentType()==null || !Constants.DIRECTION_EXP.equals(consolidationDetails.getShipmentType()) || !isNetworkTransferEntityEnabled)
                return;
            boolean isInterBranchConsole = Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole());
            boolean oldIsInterBranchConsole = oldEntity!=null && Boolean.TRUE.equals(oldEntity.getInterBranchConsole());
            if(isInterBranchConsole)
                processInterBranchEntityCase(consolidationDetails, oldEntity);
            if(!isInterBranchConsole && oldIsInterBranchConsole)
                processOldEntityInterBranch(consolidationDetails);

            if (consolidationDetails.getTransportMode()!=null && !Constants.TRANSPORT_MODE_RAI.equals(consolidationDetails.getTransportMode())) {
                processNetworkTransferEntity(consolidationDetails.getReceivingBranch(),
                        oldEntity != null ? oldEntity.getReceivingBranch() : null, consolidationDetails,
                        reverseDirection(consolidationDetails.getShipmentType()), isInterBranchConsole);

                processTriangulationPartnersForConsole(consolidationDetails, oldEntity);
            }
        } catch (Exception ex) {
            log.error("Exception during creation or updation of Network Transfer entity for Consolidation Number: {} with exception: {}", consolidationDetails.getConsolidationNumber(), ex.getMessage());
        }

    }

    private void processInterBranchEntityCase(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        boolean isConsoleBranchUpdate = oldEntity != null && oldEntity.getReceivingBranch() != null
                && !oldEntity.getReceivingBranch().equals(consolidationDetails.getReceivingBranch());

        // return if existing NTE is Accepted or update IsInterBranchEntity flag if old entity had IsInterBranchEntity flag off
        if (processNTForBranchUpdate(consolidationDetails, oldEntity, isConsoleBranchUpdate)) return;

        if (consolidationDetails.getShipmentsList() != null && !consolidationDetails.getShipmentsList().isEmpty()) {
            processInterBranchShipments(consolidationDetails, isConsoleBranchUpdate);
        } else if (oldEntity != null && !oldEntity.getShipmentsList().isEmpty()) {
            deleteNetworkTransferForOldShipments(oldEntity);
        }
    }

    public void triggerAutomaticTransfer(ConsolidationDetails consolidationDetails,
                                         ConsolidationDetails oldEntity, Boolean isDocOrHawbNumAdded) {
        try {
            if(consolidationDetails.getShipmentType()==null || !Constants.DIRECTION_EXP.equals(consolidationDetails.getShipmentType()))
                return;
            Boolean isReceivingBranchEmpty = ObjectUtils.isEmpty(consolidationDetails.getReceivingBranch()) && oldEntity != null && ObjectUtils.isNotEmpty(oldEntity.getReceivingBranch());
            if(Boolean.TRUE.equals(isReceivingBranchEmpty) || isInvalidForTransfer(consolidationDetails)) {
                deleteAllConsoleErrorsLogs(consolidationDetails);
                return;
            }

            List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurations = quartzJobInfoService.getActiveFileTransferConfigurations(consolidationDetails.getTransportMode());
            if (ObjectUtils.isEmpty(fileTransferConfigurations)) {
                deleteAllConsoleErrorsLogs(consolidationDetails);
                return;
            }

            Optional<NetworkTransfer> optionalNetworkTransfer = networkTransferDao.findByTenantAndEntity(Math.toIntExact(consolidationDetails.getReceivingBranch()), consolidationDetails.getId(), CONSOLIDATION);
            if(isNTETransferredOrAccepted(optionalNetworkTransfer)) {
                deleteAllConsoleErrorsLogs(consolidationDetails);
                return;
            }

            Optional<QuartzJobInfo> optionalQuartzJobInfo = quartzJobInfoDao.findByJobFilters(
                    consolidationDetails.getTenantId(), consolidationDetails.getId(), CONSOLIDATION);

            QuartzJobInfo quartzJobInfo = optionalQuartzJobInfo.orElse(null);

            if (isCarrierDetailsInvalid(consolidationDetails, quartzJobInfo)) return;

            if (ObjectUtils.isEmpty(quartzJobInfo) && oldEntity == null) {
                createOrUpdateQuartzJob(consolidationDetails, null);
            } else if (shouldUpdateExistingJob(quartzJobInfo, oldEntity, consolidationDetails, isDocOrHawbNumAdded, optionalNetworkTransfer)) {
                createOrUpdateQuartzJob(consolidationDetails, quartzJobInfo);
            }
        } catch (Exception e) {
            log.error("Exception during creation or updation of Automatic transfer flow for consolidation Id: {} with exception: {}", consolidationDetails.getId(), e.getMessage());
        }
    }

    private boolean processNTForBranchUpdate(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, boolean isConsoleBranchUpdate) {
        List<NetworkTransfer> networkTransferList = networkTransferDao.getInterConsoleNTList(Collections.singletonList(consolidationDetails.getId()), CONSOLIDATION);
        if(networkTransferList!=null && !networkTransferList.isEmpty()){
            for(NetworkTransfer networkTransfer: networkTransferList) {
                if(Objects.equals(networkTransfer.getJobType(), DIRECTION_CTS))
                    continue;
                if (networkTransfer.getStatus() == NetworkTransferStatus.ACCEPTED) {
                    return true;
                }
                if (!isConsoleBranchUpdate && oldEntity != null && Boolean.FALSE.equals(oldEntity.getInterBranchConsole())) {
                    networkTransfer.setIsInterBranchEntity(Boolean.TRUE);
                    networkTransferDao.save(networkTransfer);
                }
            }
        }
        return false;
    }

    private void processOldEntityInterBranch(ConsolidationDetails consolidationDetails){
        List<NetworkTransfer> networkTransferList = networkTransferDao.getInterConsoleNTList(Collections.singletonList(consolidationDetails.getId()), CONSOLIDATION);
        if(networkTransferList!=null && !networkTransferList.isEmpty()) {
            for (NetworkTransfer networkTransfer : networkTransferList) {
                if(Objects.equals(networkTransfer.getJobType(), DIRECTION_CTS))
                    continue;
                if (networkTransfer.getStatus() == NetworkTransferStatus.ACCEPTED)
                    return;
                Map<Long, Map<Integer, NetworkTransfer>> shipmentNetworkTransferMap = getNetworkTransferMap(consolidationDetails);
                List<NetworkTransfer> nteToDelete = new ArrayList<>();
                if (shipmentNetworkTransferMap != null) {
                    List<NetworkTransfer> allNetworkTransfers = shipmentNetworkTransferMap.values().stream()
                            .flatMap(innerMap -> innerMap.values().stream()).toList();
                    nteToDelete.addAll(allNetworkTransfers);
                }
                nteToDelete.forEach(networkTransferService::deleteNetworkTransferEntity);
            }
        }
    }

    private Map<Integer, NetworkTransfer> getNetworkTransferMap(ShipmentDetails shipmentDetails, Map<Long, Map<Integer, NetworkTransfer>> shipmentNetworkTransferMap) {
        return shipmentNetworkTransferMap != null
                ? shipmentNetworkTransferMap.getOrDefault(shipmentDetails.getId(), new HashMap<>())
                : null;
    }

    private Map<Long, Map<Integer, NetworkTransfer>> getNetworkTransferMap(ConsolidationDetails consolidationDetails){
        List<Long> shipmentIds = consolidationDetails.getShipmentsList().stream().map(ShipmentDetails::getId).toList();

        List<NetworkTransfer> networkTransferList = networkTransferDao.getInterConsoleNTList(shipmentIds, Constants.SHIPMENT);
        Map<Long, Map<Integer, NetworkTransfer>> shipmentNetworkTranferMap = null;
        if(networkTransferList!=null){
            shipmentNetworkTranferMap = networkTransferList.stream()
                    .collect(Collectors.groupingBy(
                            NetworkTransfer::getEntityId,
                            Collectors.toMap(NetworkTransfer::getTenantId, transfer -> transfer)
                    ));
        }
        return shipmentNetworkTranferMap;
    }

    private void processNetworkTransferEntity(Long tenantId, Long oldTenantId, ConsolidationDetails consolidationDetails, String jobType, Boolean isInterBranchConsole) {
        try{
            networkTransferService.processNetworkTransferEntity(tenantId, oldTenantId, Constants.CONSOLIDATION, null,
                    consolidationDetails, jobType, null, isInterBranchConsole);
        } catch (Exception ex) {
            log.error("Exception during processing Network Transfer entity for Consolidation Number: {} with exception: {}", consolidationDetails.getConsolidationNumber(), ex.getMessage());
        }

    }

    private String reverseDirection(String direction) {
        String res = direction;
        if(Constants.DIRECTION_EXP.equalsIgnoreCase(direction)) {
            res = Constants.DIRECTION_IMP;
        }
        else if(Constants.DIRECTION_IMP.equalsIgnoreCase(direction)) {
            res = Constants.DIRECTION_EXP;
        }
        return res;
    }

    private void processTriangulationPartnersForConsole(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        if (consolidationDetails.getTriangulationPartnerList() != null) {
            List<Long> currentPartners = commonUtils.getTriangulationPartnerList(consolidationDetails.getTriangulationPartnerList());
            List<Long> oldPartners = oldEntity != null ? commonUtils.getTriangulationPartnerList(oldEntity.getTriangulationPartnerList())
                    : Collections.emptyList();

            // Determine new tenant IDs by removing old partners from the current partners
            Set<Long> newTenantIds = new HashSet<>(currentPartners);
            newTenantIds.removeAll(oldPartners);

            // Determine old tenant IDs by removing current partners from the old partners
            Set<Long> oldTenantIds = new HashSet<>(oldPartners);
            oldTenantIds.removeAll(currentPartners);

            // Process new tenant IDs for network transfer
            newTenantIds.forEach(newTenantId -> {
                processNetworkTransferEntity(newTenantId, null, consolidationDetails, Constants.DIRECTION_CTS, false);
            });

            // Process old tenant IDs for removal from network transfer
            oldTenantIds.forEach(oldTenantId -> {
                processNetworkTransferEntity(null, oldTenantId, consolidationDetails, Constants.DIRECTION_CTS, false);
            });
        } else if (consolidationDetails.getTriangulationPartner() != null) {
            processNetworkTransferEntity(consolidationDetails.getTriangulationPartner(),
                    oldEntity != null ? oldEntity.getTriangulationPartner() : null, consolidationDetails, Constants.DIRECTION_CTS, false);
        } else if(consolidationDetails.getTriangulationPartnerList() == null) {
            List<Long> oldPartners = oldEntity != null ? commonUtils.getTriangulationPartnerList(oldEntity.getTriangulationPartnerList())
                    : Collections.emptyList();
            Set<Long> oldTenantIds = new HashSet<>(oldPartners);
            oldTenantIds.forEach(oldTenantId ->
                    processNetworkTransferEntity(null, oldTenantId, consolidationDetails, Constants.DIRECTION_CTS, false)
            );
        }
    }

    private void processInterBranchShipments(ConsolidationDetails consolidationDetails, boolean isConsoleBranchUpdate) {
        Map<Long, Map<Integer, NetworkTransfer>> shipmentNetworkTransferMap = getNetworkTransferMap(consolidationDetails);

        List<ShipmentDetails> shipmentsForNte = new ArrayList<>();
        List<ShipmentDetails> shipmentsForHiddenNte = new ArrayList<>();
        List<NetworkTransfer> nteToUpdate = new ArrayList<>();
        List<NetworkTransfer> nteToDelete = new ArrayList<>();
        NetworkTransfer existingNTE = null;

        Long consolidationReceivingBranch = consolidationDetails.getReceivingBranch();

        if (existingNteNotValid(consolidationReceivingBranch, shipmentNetworkTransferMap)) {
            List<NetworkTransfer> allNetworkTransfers = shipmentNetworkTransferMap.values().stream()
                    .flatMap(innerMap -> innerMap.values().stream()).toList();
            nteToDelete.addAll(allNetworkTransfers);
        }

        if(nteToDelete.isEmpty()) {
            for (ShipmentDetails shipmentDetails : consolidationDetails.getShipmentsList()) {
                Long receivingBranch = shipmentDetails.getReceivingBranch();
                if (receivingBranch == null) {
                    continue;
                }

                Map<Integer, NetworkTransfer> nteMap = getNetworkTransferMap(shipmentDetails, shipmentNetworkTransferMap);

                if(nteMap!=null){
                    processDbNte(nteMap, receivingBranch, nteToDelete);
                    existingNTE = nteMap.get(receivingBranch.intValue());
                }

                processConsoleBranchUpdate(isConsoleBranchUpdate, existingNTE);

                if (!Objects.equals(consolidationReceivingBranch, receivingBranch)) {
                    addNewNte(shipmentDetails, existingNTE, shipmentsForNte);
                } else if (existingNTE == null) {
                    shipmentsForHiddenNte.add(shipmentDetails);
                } else {
                    existingNTE.setIsHidden(Boolean.TRUE);
                    nteToUpdate.add(existingNTE);
                }
            }
        }

        processShipmentsLists(shipmentsForNte, shipmentsForHiddenNte, nteToUpdate, nteToDelete);
    }

    private void processShipmentsLists(List<ShipmentDetails> shipmentsForNte,
                                       List<ShipmentDetails> shipmentsForHiddenNte,
                                       List<NetworkTransfer> nteToUpdate,
                                       List<NetworkTransfer> nteToDelete) {
        nteToDelete.forEach(networkTransferService::deleteNetworkTransferEntity);

        shipmentsForNte.forEach(shipmentDetails ->
                networkTransferService.processNetworkTransferEntity(
                        shipmentDetails.getReceivingBranch(), null, SHIPMENT, shipmentDetails,
                        null, Constants.DIRECTION_IMP, null, true)
        );

        if (!shipmentsForHiddenNte.isEmpty()) {
            networkTransferService.bulkProcessInterConsoleNte(shipmentsForHiddenNte);
        }

        if (!nteToUpdate.isEmpty()) {
            networkTransferDao.saveAll(nteToUpdate);
        }
    }

    private void deleteNetworkTransferForOldShipments(ConsolidationDetails oldEntity) {
        oldEntity.getShipmentsList().forEach(shipmentDetails ->
                networkTransferService.deleteValidNetworkTransferEntity(shipmentDetails.getReceivingBranch(),
                        oldEntity.getId(), Constants.SHIPMENT)
        );
    }

    private boolean isInvalidForTransfer(ConsolidationDetails consolidationDetails) {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        return !Boolean.TRUE.equals(shipmentSettingsDetails.getIsAutomaticTransferEnabled())
                || ObjectUtils.isEmpty(consolidationDetails.getReceivingBranch())
                || (consolidationDetails.getTransportMode()!=null && Constants.TRANSPORT_MODE_RAI.equals(consolidationDetails.getTransportMode()))
                || Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole());
    }

    private void deleteAllConsoleErrorsLogs(ConsolidationDetails consolidationDetails){
        List<Long> shipmentIds = new ArrayList<>();
        if(consolidationDetails.getShipmentsList()!=null)
            shipmentIds = consolidationDetails.getShipmentsList().stream().map(BaseEntity::getId).toList();
        commonErrorLogsDao.deleteAllConsoleAndShipmentErrorsLogs(consolidationDetails.getId(), shipmentIds);
    }

    private boolean isNTETransferredOrAccepted(Optional<NetworkTransfer> optionalNetworkTransfer) {
        return optionalNetworkTransfer.isPresent() && (optionalNetworkTransfer.get().getStatus() == NetworkTransferStatus.TRANSFERRED ||
                optionalNetworkTransfer.get().getStatus() == NetworkTransferStatus.ACCEPTED);
    }

    private boolean isCarrierDetailsInvalid(ConsolidationDetails consolidationDetails, QuartzJobInfo quartzJobInfo) {
        CarrierDetails carrierDetails = consolidationDetails.getCarrierDetails();
        if (carrierDetails==null || (ObjectUtils.isEmpty(carrierDetails.getEta()) && ObjectUtils.isEmpty(carrierDetails.getEtd()) &&
                ObjectUtils.isEmpty(carrierDetails.getAta()) && ObjectUtils.isEmpty(carrierDetails.getAtd()))) {
            if(quartzJobInfo !=null && quartzJobInfo.getJobStatus() == JobState.QUEUED)
                quartzJobInfoService.deleteJobById(quartzJobInfo.getId());
            return true;
        }
        return false;
    }

    private void createOrUpdateQuartzJob(ConsolidationDetails consolidationDetails, QuartzJobInfo existingJob) {
        CarrierDetails carrierDetails = consolidationDetails.getCarrierDetails();

        LocalDateTime jobTime = quartzJobInfoService.getQuartzJobTime(
                carrierDetails.getEta(), carrierDetails.getEtd(), carrierDetails.getAta(), carrierDetails.getAtd(),
                consolidationDetails.getTransportMode());

        if(jobTime == null)
            return;

        QuartzJobInfo quartzJobInfo = (existingJob != null) ? existingJob : createNewQuartzJob(consolidationDetails);
        quartzJobInfo.setJobStatus(JobState.QUEUED);
        quartzJobInfo.setErrorMessage(null);
        quartzJobInfo.setStartTime(jobTime);

        QuartzJobInfo newQuartzJobInfo = quartzJobInfoDao.save(quartzJobInfo);

        if(existingJob!=null && quartzJobInfoService.isJobWithNamePresent(newQuartzJobInfo.getId().toString())){
            quartzJobInfoService.updateSimpleJob(newQuartzJobInfo);
        }else{
            quartzJobInfoService.createSimpleJob(newQuartzJobInfo);
        }
        deleteAllConsoleErrorsLogs(consolidationDetails);
    }

    private QuartzJobInfo createNewQuartzJob(ConsolidationDetails consolidationDetails) {
        return QuartzJobInfo.builder()
                .entityId(consolidationDetails.getId())
                .entityType(CONSOLIDATION)
                .tenantId(consolidationDetails.getTenantId())
                .jobType(JobType.SIMPLE_JOB)
                .build();
    }

    private boolean shouldUpdateExistingJob(QuartzJobInfo quartzJobInfo, ConsolidationDetails oldEntity, ConsolidationDetails consolidationDetails, Boolean isDocAdded, Optional<NetworkTransfer> optionalNetworkTransfer) {

        return (isValidforAutomaticTransfer(quartzJobInfo, consolidationDetails, oldEntity, isDocAdded))
                || (isValidReceivingBranchChange(consolidationDetails, oldEntity, optionalNetworkTransfer));
    }

    private boolean isValidforAutomaticTransfer(QuartzJobInfo quartzJobInfo, ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, Boolean isDocAdded) {
        if (isValidDateChange(consolidationDetails, oldEntity))
            return true;

        if(quartzJobInfo==null ||(quartzJobInfo.getJobStatus() != JobState.ERROR))
            return false;

        if(Boolean.TRUE.equals(isDocAdded))
            return true;

        CarrierDetails newCarrierDetails = consolidationDetails.getCarrierDetails();

        // If oldCarrierDetails is null, check if newCarrierDetails has any populated fields.
        if (oldEntity == null || oldEntity.getCarrierDetails() == null) {
            return newCarrierDetails.getEta() != null ||
                    newCarrierDetails.getEtd() != null ||
                    newCarrierDetails.getAta() != null ||
                    newCarrierDetails.getAtd() != null;
        }

        if (isAirStandardCase(consolidationDetails)) {
            return isAirStandardCaseChanged(oldEntity, newCarrierDetails);
        }
        if (isAirNonStandardNonDrtCase(consolidationDetails)) {
            return isAirNonStandardNonDrtCaseChanged(consolidationDetails, oldEntity, newCarrierDetails);
        }

        if (isSeaCase(consolidationDetails)) {
            return isSeaCaseChanged(consolidationDetails, oldEntity, newCarrierDetails);
        }

        // Compare individual fields for changes.
        return isDefaultCaseChanged(consolidationDetails, oldEntity, newCarrierDetails);
    }

    private boolean isAirStandardCaseChanged(ConsolidationDetails oldEntity, CarrierDetails newCarrierDetails) {
        CarrierDetails oldCarrierDetails = oldEntity.getCarrierDetails();
        return isValueChanged(newCarrierDetails.getFlightNumber(), oldCarrierDetails.getFlightNumber()) ||
                isValueChanged(newCarrierDetails.getEta(), oldCarrierDetails.getEta()) ||
                isValueChanged(newCarrierDetails.getEtd(), oldCarrierDetails.getEtd());
    }

    private boolean isAirStandardCase(ConsolidationDetails consolidationDetails) {
        return Objects.equals(consolidationDetails.getTransportMode(), TRANSPORT_MODE_AIR) &&
                Objects.equals(consolidationDetails.getConsolidationType(), SHIPMENT_TYPE_STD);
    }

    private boolean isAirNonStandardNonDrtCaseChanged(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, CarrierDetails newCarrierDetails) {
        CarrierDetails oldCarrierDetails = oldEntity.getCarrierDetails();
        return isValueChanged(newCarrierDetails.getFlightNumber(), oldCarrierDetails.getFlightNumber()) ||
                isValueChanged(newCarrierDetails.getEta(), oldCarrierDetails.getEta()) ||
                isValueChanged(newCarrierDetails.getEtd(), oldCarrierDetails.getEtd()) ||
                isValueChanged(consolidationDetails.getBol(), oldEntity.getBol());
    }

    private boolean isAirNonStandardNonDrtCase(ConsolidationDetails consolidationDetails) {
        return Objects.equals(consolidationDetails.getTransportMode(), TRANSPORT_MODE_AIR) &&
                !Objects.equals(consolidationDetails.getConsolidationType(), SHIPMENT_TYPE_STD) &&
                !Objects.equals(consolidationDetails.getConsolidationType(), CONSOLIDATION_TYPE_DRT);
    }

    private boolean isSeaCaseChanged(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, CarrierDetails newCarrierDetails) {
        CarrierDetails oldCarrierDetails = oldEntity.getCarrierDetails();
        return isValueChanged(newCarrierDetails.getFlightNumber(), oldCarrierDetails.getFlightNumber()) ||
                isValueChanged(newCarrierDetails.getEta(), oldCarrierDetails.getEta()) ||
                isValueChanged(newCarrierDetails.getEtd(), oldCarrierDetails.getEtd()) ||
                isValueChanged(newCarrierDetails.getVessel(), oldCarrierDetails.getVessel()) ||
                isValueChanged(newCarrierDetails.getShippingLine(), oldCarrierDetails.getShippingLine()) ||
                isValueChanged(newCarrierDetails.getVoyage(), oldCarrierDetails.getVoyage()) ||
                isAgentChanged(consolidationDetails, oldEntity) ||
                isValueChanged(consolidationDetails.getBol(), oldEntity.getBol());
    }

    private boolean isSeaCase(ConsolidationDetails consolidationDetails) {
        return Objects.equals(consolidationDetails.getTransportMode(), TRANSPORT_MODE_SEA);
    }

    private boolean isAgentChanged(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity) {
        return (consolidationDetails.getSendingAgent() != null && oldEntity.getSendingAgent() == null) ||
                (consolidationDetails.getSendingAgent() != null && oldEntity.getSendingAgent() != null &&
                        isValueChanged(consolidationDetails.getSendingAgent().getOrgCode(), oldEntity.getSendingAgent().getOrgCode())) ||
                (consolidationDetails.getReceivingAgent() != null && oldEntity.getReceivingAgent() == null) ||
                (consolidationDetails.getReceivingAgent() != null && oldEntity.getReceivingAgent() != null &&
                        isValueChanged(consolidationDetails.getReceivingAgent().getOrgCode(), oldEntity.getReceivingAgent().getOrgCode()));
    }

    private boolean isDefaultCaseChanged(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, CarrierDetails newCarrierDetails) {
        CarrierDetails oldCarrierDetails = oldEntity.getCarrierDetails();
        return isValueChanged(newCarrierDetails.getEta(), oldCarrierDetails.getEta()) ||
                isValueChanged(newCarrierDetails.getEtd(), oldCarrierDetails.getEtd()) ||
                isAgentChanged(consolidationDetails, oldEntity);
    }

    private boolean isValueChanged(Object newValue, Object oldValue) {
        return (oldValue != null && newValue==null) || (newValue != null && !newValue.equals(oldValue));
    }

    private boolean isValidDateChange(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity){
        CarrierDetails newCarrierDetails = consolidationDetails.getCarrierDetails();
        if(oldEntity!=null && oldEntity.getCarrierDetails()!=null && newCarrierDetails!=null){
            CarrierDetails oldCarrierDetails = oldEntity.getCarrierDetails();
            return isValueChanged(newCarrierDetails.getEta(), oldCarrierDetails.getEta())
                    || isValueChanged(newCarrierDetails.getEtd(), oldCarrierDetails.getEtd())
                    || isValueChanged(newCarrierDetails.getAta(), oldCarrierDetails.getAta())
                    || isValueChanged(newCarrierDetails.getAtd(), oldCarrierDetails.getAtd());
        }
        return false;
    }

    private boolean isValidReceivingBranchChange(ConsolidationDetails consolidationDetails, ConsolidationDetails oldEntity, Optional<NetworkTransfer> optionalNetworkTransfer) {

        if (oldEntity == null) {
            return false;
        }

        if (oldEntity.getReceivingBranch()==null) {
            return true;
        }

        boolean isBranchChanged = !Objects.equals(oldEntity.getReceivingBranch(), consolidationDetails.getReceivingBranch());
        if (!isBranchChanged) {
            return false;
        }

        Optional<NetworkTransfer> oldOptionalNetworkTransfer = networkTransferDao.findByTenantAndEntity(
                Math.toIntExact(oldEntity.getReceivingBranch()), oldEntity.getId(), CONSOLIDATION);

        if(oldOptionalNetworkTransfer.isEmpty())
            return true;

        return oldOptionalNetworkTransfer
                .map(networkTransfer -> networkTransfer.getStatus() != NetworkTransferStatus.ACCEPTED)
                .orElse(false) || optionalNetworkTransfer.isEmpty();
    }

    private boolean existingNteNotValid(Long consolidationReceivingBranch, Map<Long, Map<Integer, NetworkTransfer>> shipmentNetworkTransferMap) {
        return consolidationReceivingBranch == null && shipmentNetworkTransferMap != null;
    }

    private void processDbNte(Map<Integer, NetworkTransfer> nteMap, Long receivingBranch, List<NetworkTransfer> nteToDelete) {
        NetworkTransfer dbNte = nteMap.values().stream().findFirst().orElse(null);
        if(dbNte!=null && dbNte.getTenantId()!= receivingBranch.intValue()){
            nteToDelete.add(dbNte);
        }
    }

    private void processConsoleBranchUpdate(boolean isConsoleBranchUpdate, NetworkTransfer existingNTE){
        if (isConsoleBranchUpdate && existingNTE != null && existingNTE.getEntityPayload() != null
                && existingNTE.getStatus() != NetworkTransferStatus.ACCEPTED) {
            if(existingNTE.getStatus() != NetworkTransferStatus.REASSIGNED)
                existingNTE.setStatus(NetworkTransferStatus.SCHEDULED);
            existingNTE.setEntityPayload(null);
            networkTransferDao.save(existingNTE);
        }
    }

    private void addNewNte(ShipmentDetails shipmentDetails, NetworkTransfer existingNTE, List<ShipmentDetails> shipmentsForNte) {
        if (existingNTE == null) {
            shipmentsForNte.add(shipmentDetails);
        }
    }

    public void createOrUpdateNetworkTransferEntity(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        try{
            if(shipmentDetails.getDirection()!=null && Constants.DIRECTION_EXP.equals(shipmentDetails.getDirection()))
                processInterBranchEntityCase(shipmentDetails, oldEntity);
            if (isInterBranchConsole(shipmentDetails) || oldEntityHasInterBranchConsole(oldEntity))
                return;

            // Check if the shipment is eligible for network transfer
            if (isEligibleForNetworkTransfer(shipmentDetails)) {

                // Process the receiving branch for network transfer
                processNetworkTransferEntity(shipmentDetails.getReceivingBranch(),
                        oldEntity != null ? oldEntity.getReceivingBranch() : null, shipmentDetails,
                        reverseDirection(shipmentDetails.getDirection()));

                if (shipmentDetails.getTriangulationPartnerList() != null) {
                    processTriangulationPartnerList(shipmentDetails, oldEntity);
                } else if (shipmentDetails.getTriangulationPartner() != null) {
                    processEmptyTriangulationPartner(shipmentDetails, oldEntity);
                } else if(shipmentDetails.getTriangulationPartnerList() == null) {
                    processEmptyTriangulationPartnerList(shipmentDetails, oldEntity);
                }

            } else {
                // If not eligible for network transfer, handle deletion of old network transfer entities
                processNonEligibleNTE(oldEntity);

            }
        } catch (Exception ex) {
            log.error("Exception during creation or updation of Network Transfer entity for shipment Id: {} with exception: {}", shipmentDetails.getShipmentId(), ex.getMessage());
        }
    }

    private void processEmptyTriangulationPartner(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        processNetworkTransferEntity(shipmentDetails.getTriangulationPartner(),
                oldEntity != null ? oldEntity.getTriangulationPartner() : null, shipmentDetails,
                Constants.DIRECTION_CTS);
    }

    private void processNonEligibleNTE(ShipmentDetails oldEntity) {
        if(oldEntity !=null && oldEntity.getReceivingBranch() != null)
            networkTransferService.deleteValidNetworkTransferEntity(oldEntity.getReceivingBranch(),
                    oldEntity.getId(), Constants.SHIPMENT);

        // Delete network transfer entries for old triangulation partners
        if (oldEntity != null && ObjectUtils.isNotEmpty(oldEntity.getTriangulationPartnerList())) {
            for (TriangulationPartner triangularPartner : oldEntity.getTriangulationPartnerList()) {
                if (triangularPartner != null)
                    networkTransferService.deleteValidNetworkTransferEntity(triangularPartner.getTriangulationPartner(),
                            oldEntity.getId(), Constants.SHIPMENT);
            }
        } else if (oldEntity != null && oldEntity.getTriangulationPartnerList() == null
                && oldEntity.getTriangulationPartner() != null) {
            networkTransferService.deleteValidNetworkTransferEntity(oldEntity.getTriangulationPartner(),
                    oldEntity.getId(), Constants.SHIPMENT);
        }
    }

    private void processEmptyTriangulationPartnerList(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        List<Long> oldPartners = oldEntity != null ? commonUtils.getTriangulationPartnerList(oldEntity.getTriangulationPartnerList())
                : Collections.emptyList();
        Set<Long> oldTenantIds = new HashSet<>(oldPartners);
        oldTenantIds.forEach(oldTenantId -> processNetworkTransferEntity(null, oldTenantId, shipmentDetails, Constants.DIRECTION_CTS));
    }

    private void processTriangulationPartnerList(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        // Retrieve current and old triangulation partners
        List<Long> currentPartners = commonUtils.getTriangulationPartnerList(shipmentDetails.getTriangulationPartnerList());
        List<Long> oldPartners = oldEntity != null ? commonUtils.getTriangulationPartnerList(oldEntity.getTriangulationPartnerList())
                : Collections.emptyList();

        // Determine new tenant IDs by removing old partners from the current partners
        Set<Long> newTenantIds = new HashSet<>(currentPartners);
        newTenantIds.removeAll(oldPartners);

        // Determine old tenant IDs by removing current partners from the old partners
        Set<Long> oldTenantIds = new HashSet<>(oldPartners);
        oldTenantIds.removeAll(currentPartners);

        // Process new tenant IDs for network transfer
        newTenantIds.forEach(newTenantId -> processNetworkTransferEntity(newTenantId, null, shipmentDetails, Constants.DIRECTION_CTS));

        // Process old tenant IDs for removal from network transfer
        oldTenantIds.forEach(oldTenantId -> processNetworkTransferEntity(null, oldTenantId, shipmentDetails, Constants.DIRECTION_CTS));
    }

    public void triggerAutomaticTransfer(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, Boolean isDocAdded) {
        try{
            if (isInterBranchConsole(shipmentDetails) || oldEntityHasInterBranchConsole(oldEntity))
                return;
            Optional<QuartzJobInfo> optionalQuartzJobInfo = quartzJobInfoDao.findByJobFilters(
                    shipmentDetails.getTenantId(), shipmentDetails.getId(), SHIPMENT);

            QuartzJobInfo quartzJobInfo = optionalQuartzJobInfo.orElse(null);

            if(isEligibleForNetworkTransfer(shipmentDetails) && ObjectUtils.isNotEmpty(shipmentDetails.getReceivingBranch())){

                if (automaticTriggerNotValid(shipmentDetails, quartzJobInfo)) return;

                if (shouldCreateOrUpdateQuartzJob(quartzJobInfo, oldEntity, shipmentDetails, isDocAdded)) {
                    createOrUpdateQuartzJob(shipmentDetails, quartzJobInfo);
                }
            } else if (isEligibleForNetworkTransfer(shipmentDetails) && ObjectUtils.isEmpty(shipmentDetails.getReceivingBranch())
                    && oldEntity != null && ObjectUtils.isNotEmpty(oldEntity.getReceivingBranch())) {
                commonErrorLogsDao.deleteShipmentErrorsLogs(shipmentDetails.getId());
            }
            if (hasHouseBillChange(shipmentDetails, oldEntity) || hasMasterBillChange(shipmentDetails, oldEntity)) {
                triggerConsoleTransfer(shipmentDetails);
            }
        } catch (Exception e) {
            log.error("Exception during creation or updation of Automatic transfer flow for shipment Id: {} with exception: {}", shipmentDetails.getShipmentId(), e.getMessage());
        }
    }

    private boolean automaticTriggerNotValid(ShipmentDetails shipmentDetails, QuartzJobInfo quartzJobInfo) {
        if (isInvalidNetworkTransfer(shipmentDetails)){
            commonErrorLogsDao.deleteShipmentErrorsLogs(shipmentDetails.getId());
            return true;
        }

        List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurations = quartzJobInfoService.getActiveFileTransferConfigurations(shipmentDetails.getTransportMode());
        if (ObjectUtils.isEmpty(fileTransferConfigurations)) {
            commonErrorLogsDao.deleteShipmentErrorsLogs(shipmentDetails.getId());
            return true;
        }

        if (isCarrierDetailsInvalid(shipmentDetails)) {
            handleInvalidCarrierDetails(quartzJobInfo);
            return true;
        }
        return false;
    }

    private boolean isInvalidNetworkTransfer(ShipmentDetails shipmentDetails) {
        Optional<NetworkTransfer> optionalNetworkTransfer = networkTransferDao.findByTenantAndEntity(
                Math.toIntExact(shipmentDetails.getReceivingBranch()), shipmentDetails.getId(), SHIPMENT);
        return optionalNetworkTransfer.isPresent() &&
                (optionalNetworkTransfer.get().getStatus() == NetworkTransferStatus.TRANSFERRED ||
                        optionalNetworkTransfer.get().getStatus() == NetworkTransferStatus.ACCEPTED);
    }

    private boolean isCarrierDetailsInvalid(ShipmentDetails shipmentDetails) {
        CarrierDetails carrierDetails = shipmentDetails.getCarrierDetails();
        return carrierDetails == null || (
                ObjectUtils.isEmpty(carrierDetails.getEta()) &&
                        ObjectUtils.isEmpty(carrierDetails.getEtd()) &&
                        ObjectUtils.isEmpty(carrierDetails.getAta()) &&
                        ObjectUtils.isEmpty(carrierDetails.getAtd()));
    }

    private void handleInvalidCarrierDetails(QuartzJobInfo quartzJobInfo) {
        if (quartzJobInfo != null && quartzJobInfo.getJobStatus() == JobState.QUEUED) {
            quartzJobInfoService.deleteJobById(quartzJobInfo.getId());
        }
    }

    private boolean shouldCreateOrUpdateQuartzJob(QuartzJobInfo quartzJobInfo, ShipmentDetails oldEntity, ShipmentDetails shipmentDetails, Boolean isDocAdded) {
        return (quartzJobInfo == null && oldEntity == null) ||
                shouldUpdateExistingJob(quartzJobInfo, oldEntity, shipmentDetails, isDocAdded);
    }

    private void triggerConsoleTransfer(ShipmentDetails shipmentDetails){
        if(ObjectUtils.isEmpty(shipmentDetails.getConsolidationList())){
            return;
        }

        for(ConsolidationDetails consolidationDetails: shipmentDetails.getConsolidationList()){
            Optional<QuartzJobInfo> optionalQuartzJobInfo = quartzJobInfoDao.findByJobFilters(
                    consolidationDetails.getTenantId(), consolidationDetails.getId(), CONSOLIDATION);

            QuartzJobInfo quartzJobInfo = optionalQuartzJobInfo.orElse(null);
            if(quartzJobInfo!=null && quartzJobInfo.getJobStatus()==JobState.ERROR){
                if(TRANSPORT_MODE_AIR.equals(consolidationDetails.getTransportMode()) &&
                        !Objects.equals(shipmentDetails.getJobType(), SHIPMENT_TYPE_DRT) &&
                        !Objects.equals(shipmentDetails.getJobType(), SHIPMENT_TYPE_STD)) {
                    triggerAutomaticTransfer(consolidationDetails, null, true);
                }
                if(TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode()) &&
                        !Objects.equals(shipmentDetails.getJobType(), SHIPMENT_TYPE_DRT)) {
                    triggerAutomaticTransfer(consolidationDetails, null, true);
                }
            }
        }
    }

    private boolean hasHouseBillChange(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        return (oldEntity == null && shipmentDetails.getHouseBill() != null)
                || (oldEntity != null && isValueChanged(shipmentDetails.getHouseBill(), oldEntity.getHouseBill()));
    }

    private boolean hasMasterBillChange(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        return (oldEntity == null && shipmentDetails.getMasterBill() != null)
                || (oldEntity != null && isValueChanged(shipmentDetails.getMasterBill(), oldEntity.getMasterBill()));
    }

    private void createOrUpdateQuartzJob(ShipmentDetails shipmentDetails, QuartzJobInfo existingJob) {
        CarrierDetails carrierDetails = shipmentDetails.getCarrierDetails();

        LocalDateTime jobTime = quartzJobInfoService.getQuartzJobTime(
                carrierDetails.getEta(), carrierDetails.getEtd(), carrierDetails.getAta(), carrierDetails.getAtd(),
                shipmentDetails.getTransportMode());

        if(jobTime == null)
            return;

        QuartzJobInfo quartzJobInfo = (existingJob != null) ? existingJob : createNewQuartzJob(shipmentDetails);
        quartzJobInfo.setJobStatus(JobState.QUEUED);
        quartzJobInfo.setStartTime(jobTime);
        quartzJobInfo.setErrorMessage(null);
        QuartzJobInfo newQuartzJobInfo = quartzJobInfoDao.save(quartzJobInfo);

        if(existingJob!=null && newQuartzJobInfo.getId()!=null && quartzJobInfoService.isJobWithNamePresent(newQuartzJobInfo.getId().toString())){
            quartzJobInfoService.updateSimpleJob(newQuartzJobInfo);
        }else{
            quartzJobInfoService.createSimpleJob(newQuartzJobInfo);
        }
        commonErrorLogsDao.deleteShipmentErrorsLogs(shipmentDetails.getId());
    }

    private QuartzJobInfo createNewQuartzJob(ShipmentDetails shipmentDetails) {
        return QuartzJobInfo.builder()
                .entityId(shipmentDetails.getId())
                .entityType(SHIPMENT)
                .tenantId(shipmentDetails.getTenantId())
                .jobType(JobType.SIMPLE_JOB)
                .build();
    }

    private boolean shouldUpdateExistingJob(QuartzJobInfo quartzJobInfo, ShipmentDetails oldEntity, ShipmentDetails shipmentDetails, Boolean isDocAdded) {
        return (isValidforAutomaticTransfer(quartzJobInfo, shipmentDetails, oldEntity, isDocAdded))
                || (isValidReceivingBranchChange(shipmentDetails, oldEntity));
    }

    private boolean isValidforAutomaticTransfer(QuartzJobInfo quartzJobInfo, ShipmentDetails shipmentDetails, ShipmentDetails oldEntity, Boolean isDocAdded) {
        if (isValidDateChange(shipmentDetails, oldEntity))
            return true;

        if(quartzJobInfo==null ||( quartzJobInfo.getJobStatus() != JobState.ERROR))
            return false;

        if(Boolean.TRUE.equals(isDocAdded))
            return true;

        CarrierDetails newCarrierDetails = shipmentDetails.getCarrierDetails();

        // If oldCarrierDetails is null, check if newCarrierDetails has any populated fields.
        if (oldEntity == null || oldEntity.getCarrierDetails()==null) {
            return newCarrierDetails.getEta() != null
                    || newCarrierDetails.getEtd() != null
                    || newCarrierDetails.getAta() != null
                    || newCarrierDetails.getAtd() != null;
        }

        CarrierDetails oldCarrierDetails = oldEntity.getCarrierDetails();
        // Compare individual fields for changes.
        return isValueChanged(newCarrierDetails.getFlightNumber(), oldCarrierDetails.getFlightNumber());
    }

    private boolean isValidDateChange(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity){
        CarrierDetails newCarrierDetails = shipmentDetails.getCarrierDetails();
        if(oldEntity!=null && oldEntity.getCarrierDetails()!=null && newCarrierDetails!=null){
            CarrierDetails oldCarrierDetails = oldEntity.getCarrierDetails();
            return isValueChanged(newCarrierDetails.getEta(), oldCarrierDetails.getEta())
                    || isValueChanged(newCarrierDetails.getEtd(), oldCarrierDetails.getEtd())
                    || isValueChanged(newCarrierDetails.getAta(), oldCarrierDetails.getAta())
                    || isValueChanged(newCarrierDetails.getAtd(), oldCarrierDetails.getAtd());
        }
        return false;
    }

    private boolean isValidReceivingBranchChange(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (oldEntity == null) {
            return false;
        }

        if (oldEntity.getReceivingBranch()==null) {
            return true;
        }

        boolean isBranchChanged = !Objects.equals(oldEntity.getReceivingBranch(), shipmentDetails.getReceivingBranch());
        if (!isBranchChanged) {
            return false;
        }

        Optional<NetworkTransfer> oldOptionalNetworkTransfer = networkTransferDao.findByTenantAndEntity(
                Math.toIntExact(oldEntity.getReceivingBranch()), oldEntity.getId(), SHIPMENT);

        return ((oldOptionalNetworkTransfer.isEmpty()) || oldOptionalNetworkTransfer
                .map(networkTransfer -> networkTransfer.getStatus() != NetworkTransferStatus.ACCEPTED)
                .orElse(false));
    }

    private boolean isEligibleForNetworkTransfer(ShipmentDetails details) {
        return TRANSPORT_MODE_AIR.equals(details.getTransportMode())
                && Constants.SHIPMENT_TYPE_DRT.equals(details.getJobType()) && Constants.DIRECTION_EXP.equals(details.getDirection());
    }

    private void processInterBranchEntityCase(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        if (isInterBranchConsole(shipmentDetails)) {
            processReceivingBranchChanges(shipmentDetails, oldEntity);
        } else if ((shipmentDetails.getConsolidationList()==null || shipmentDetails.getConsolidationList().isEmpty()) && oldEntityHasInterBranchConsole(oldEntity)) {
            deleteOldConsolidationTransfers(oldEntity);
        }
    }

    private boolean isInterBranchConsole(ShipmentDetails shipmentDetails) {
        return shipmentDetails.getConsolidationList() != null
                && shipmentDetails.getConsolidationList().stream().anyMatch(consolidation -> Boolean.TRUE.equals(consolidation.getInterBranchConsole()));
    }

    private boolean oldEntityHasInterBranchConsole(ShipmentDetails oldEntity) {
        return oldEntity != null && oldEntity.getConsolidationList() != null &&
                oldEntity.getConsolidationList().stream().anyMatch(consolidation -> Boolean.TRUE.equals(consolidation.getInterBranchConsole()));
    }

    private boolean isConsoleAccepted(ConsolidationDetails console){
        List<NetworkTransfer> networkTransferList = networkTransferDao.getInterConsoleNTList(Collections.singletonList(console.getId()), CONSOLIDATION);
        if(networkTransferList!=null && !networkTransferList.isEmpty() ){
            for(NetworkTransfer networkTransfer: networkTransferList) {
                if(Objects.equals(networkTransfer.getJobType(), DIRECTION_CTS))
                    continue;
                if(networkTransfer.getStatus()==NetworkTransferStatus.ACCEPTED)
                    return true;
            }
        }
        return false;
    }

    private void processReceivingBranchChanges(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        ConsolidationDetails consolidationDetails = shipmentDetails.getConsolidationList().iterator().next();
        boolean isConsoleAcceptedCase = isConsoleAccepted(consolidationDetails);
        if(isConsoleAcceptedCase){
            return;
        }
        if(consolidationDetails.getReceivingBranch()!=null) {
            List<Long> shipmentIdsList = shipmentDetails.getConsolidationList().iterator().next().getShipmentsList().stream()
                    .map(ShipmentDetails::getId).toList();
            List<NetworkTransfer> nteList = networkTransferDao.getInterConsoleNTList(shipmentIdsList, SHIPMENT);
            Map<Long, NetworkTransfer> shipmentNetworkTransferMap = nteList!=null ? nteList.stream()
                    .collect(Collectors.toMap(NetworkTransfer::getEntityId, transfer -> transfer)) : null;
            NetworkTransfer existingNTE = shipmentNetworkTransferMap != null ? shipmentNetworkTransferMap.get(shipmentDetails.getId()) : null;
            if(existingNTE!=null && existingNTE.getStatus() == NetworkTransferStatus.ACCEPTED)
                return;
            if (shipmentDetails.getReceivingBranch() != null) {
                handleReceivingBranchUpdates(shipmentDetails, oldEntity, consolidationDetails, existingNTE, shipmentNetworkTransferMap);
            } else if (shouldDeleteOldTransfer(oldEntity, existingNTE)) {
                networkTransferService.deleteNetworkTransferEntity(existingNTE);
            }
        }else{
            List<NetworkTransfer> nteList = networkTransferDao.getInterConsoleNTList(Collections.singletonList(shipmentDetails.getId()), SHIPMENT);
            if(nteList!=null){
                nteList.forEach(nte -> networkTransferService.deleteNetworkTransferEntity(nte));
            }
        }
    }

    private void handleReceivingBranchUpdates(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity,
                                              ConsolidationDetails consolidationDetails, NetworkTransfer existingNTE,
                                              Map<Long, NetworkTransfer> shipmentNetworkTransferMap) {
        if (isBranchChanged(shipmentDetails, oldEntity)) {
            processBranchChangeOrUpdate(shipmentDetails, oldEntity, consolidationDetails, existingNTE, shipmentNetworkTransferMap);
        } else if (shouldUpdateConsole(shipmentDetails, consolidationDetails)) {
            processNetworkTransferEntity(shipmentDetails, existingNTE);
        } else {
            networkTransferService.deleteValidNetworkTransferEntity(shipmentDetails.getReceivingBranch(),
                    shipmentDetails.getId(), Constants.SHIPMENT);
        }
    }

    private boolean isBranchChanged(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity) {
        return oldEntity == null || oldEntity.getReceivingBranch()==null || (
                oldEntity.getReceivingBranch() != null &&
                        !Objects.equals(oldEntity.getReceivingBranch(), shipmentDetails.getReceivingBranch()));
    }

    private boolean shouldDeleteOldTransfer(ShipmentDetails oldEntity, NetworkTransfer existingNTE) {
        return oldEntity != null && oldEntity.getReceivingBranch() != null && existingNTE!=null &&
                Boolean.TRUE.equals(existingNTE.getIsInterBranchEntity());
    }

    private boolean shouldUpdateConsole(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails) {
        return !Objects.equals(shipmentDetails.getReceivingBranch(), consolidationDetails.getReceivingBranch());
    }

    private void processBranchChangeOrUpdate(ShipmentDetails shipmentDetails, ShipmentDetails oldEntity,
                                             ConsolidationDetails consolidationDetails, NetworkTransfer existingNTE,
                                             Map<Long, NetworkTransfer> shipmentNetworkTransferMap) {
        if (!Objects.equals(shipmentDetails.getReceivingBranch(), consolidationDetails.getReceivingBranch())) {
            Long oldReceivingBranch = oldEntity!=null? oldEntity.getReceivingBranch(): null;
            networkTransferService.processNetworkTransferEntity(shipmentDetails.getReceivingBranch(), oldReceivingBranch,
                    Constants.SHIPMENT, shipmentDetails, null, reverseDirection(shipmentDetails.getDirection()), null, true);
        } else {
            if (existingNTE!=null) {
                networkTransferService.deleteNetworkTransferEntity(existingNTE);
                networkTransferService.bulkProcessInterConsoleNte(Collections.singletonList(shipmentDetails));
            } else {
                networkTransferService.bulkProcessInterConsoleNte(Collections.singletonList(shipmentDetails));
            }
        }
        // empty entity payload here
        updateNetworkTransfersForShipments(shipmentDetails.getId(), shipmentNetworkTransferMap);
        updateConsoleNetworkTransfer(consolidationDetails);
    }

    private void updateNetworkTransfersForShipments(Long currentShipmentId, Map<Long, NetworkTransfer> shipmentNetworkTransferMap) {
        if(shipmentNetworkTransferMap==null)
            return;
        shipmentNetworkTransferMap.values().stream()
                .filter(networkTransfer -> !Objects.equals(networkTransfer.getEntityId(), currentShipmentId))
                .forEach(this::resetNetworkTransferIfNeeded);
    }

    private void resetNetworkTransferIfNeeded(NetworkTransfer networkTransfer) {
        if (networkTransfer.getEntityPayload() != null && !networkTransfer.getEntityPayload().isEmpty()
                && networkTransfer.getStatus() != NetworkTransferStatus.ACCEPTED) {
            if(networkTransfer.getStatus() != NetworkTransferStatus.REASSIGNED)
                networkTransfer.setStatus(NetworkTransferStatus.SCHEDULED);
            networkTransfer.setEntityPayload(null);
            networkTransferDao.save(networkTransfer);
        }
    }

    private void updateConsoleNetworkTransfer(ConsolidationDetails consolidationDetails) {
        Optional<NetworkTransfer> optionalConsoleNetworkTransfer = networkTransferDao
                .getInterConsoleNTList(Collections.singletonList(consolidationDetails.getId()), CONSOLIDATION).stream().findFirst();
        optionalConsoleNetworkTransfer.ifPresent(this::resetNetworkTransferIfNeeded);
    }

    private void processNetworkTransferEntity(ShipmentDetails shipmentDetails, NetworkTransfer existingNTE) {
        Long oldTenantId = (existingNTE != null && Objects.equals(existingNTE.getTenantId(), shipmentDetails.getReceivingBranch().intValue()))
                ? Long.valueOf(existingNTE.getTenantId()) : null;
        networkTransferService.processNetworkTransferEntity(shipmentDetails.getReceivingBranch(), oldTenantId, Constants.SHIPMENT,
                shipmentDetails, null, reverseDirection(shipmentDetails.getDirection()), null, true);
    }

    private void processNetworkTransferEntity(Long tenantId, Long oldTenantId, ShipmentDetails shipmentDetails, String jobType) {
        try {
            networkTransferService.processNetworkTransferEntity(tenantId, oldTenantId, Constants.SHIPMENT, shipmentDetails,
                    null, jobType, null, false);
        } catch (Exception ex) {
            log.error("Exception during processing Network Transfer entity for shipment Id: {} with exception: {}", shipmentDetails.getShipmentId(), ex.getMessage());
        }
    }

    private void deleteOldConsolidationTransfers(ShipmentDetails oldEntity) {
        List<NetworkTransfer> networkTransferList = networkTransferDao.getInterConsoleNTList(Collections.singletonList(oldEntity.getId()), SHIPMENT);
        if(networkTransferList!=null) {
            for (NetworkTransfer networkTransfer : networkTransferList) {
                networkTransferService.deleteNetworkTransferEntity(networkTransfer);
            }
        }
    }

}
