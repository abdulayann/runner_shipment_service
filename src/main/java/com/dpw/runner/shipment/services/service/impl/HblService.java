package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.HblGenerateRequest;
import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import com.dpw.runner.shipment.services.dto.request.HblRequest;
import com.dpw.runner.shipment.services.dto.request.HblResetRequest;
import com.dpw.runner.shipment.services.dto.request.hbl.HblCargoDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.response.HblResponse;
import com.dpw.runner.shipment.services.dto.v1.response.CompanySettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.HblReset;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IHblService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.HblRequestV2;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IHblSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;


@Slf4j
@Service
public class HblService implements IHblService {

    @Autowired
    private IHblDao hblDao;
    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private IShipmentSync shipmentSync;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    IContainerDao containerDao;
    @Autowired
    IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private SyncConfig syncConfig;
    @Autowired
    private MasterDataUtils masterDataUtil;

    @Autowired
    @Lazy
    IShipmentService shipmentService;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private PartialFetchUtils partialFetchUtils;

    @Autowired
    private ConsolidationService consolidationService;

    @Autowired
    private HblService self;

    @Value("${v1service.url.base}${v1service.url.hblSync}")
    private String hblV1SyncUrl;

    @Autowired
    private IHblSync hblSync;

    @Override
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        HblRequest request = null;
        request = (HblRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Hbl create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Hbl hbl = convertRequestToEntity(request);
        try {
            hbl = hblDao.save(hbl);
            hblSync(hbl);
            log.info("Hbl Details created successfully for Id {} with Request Id {}", hbl.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(hbl));
    }

    private void hblSync(Hbl hbl) {
        try {
            hblSync.sync(hbl, UUID.randomUUID().toString());
        }
        catch (Exception e) {
            log.error(SyncingConstants.ERROR_PERFORMING_HBL_SYNC, e);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        HblRequest request = (HblRequest) commonRequestModel.getData();
        long id = request.getId();
        Optional<Hbl> oldEntity = hblDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Hbl is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        Hbl hbl = convertRequestToEntity(request);
        Hbl old = oldEntity.get();
        old.setHblData(hbl.getHblData());
        old.setHblCargo(hbl.getHblCargo());
        old.setHblContainer(hbl.getHblContainer());
        old.setHblNotifyParty(hbl.getHblNotifyParty());
        try {
            hbl = hblDao.save(old);
            hblSync(hbl);
            log.info("Updated the Hbl details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(hbl));
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        Long id = ((CommonGetRequest)commonRequestModel.getData()).getId();
        Optional<Hbl> hbl = hblDao.findById(id);
        if (hbl.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
       CommonGetRequest request=((CommonGetRequest)commonRequestModel.getData());
        if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
            return ResponseHelper.buildSuccessResponse(convertEntityToDto(hbl.get()));
        else
            return ResponseHelper.buildSuccessResponse(partialFetchUtils.fetchPartialListData(convertEntityToDto(hbl.get()),request.getIncludeColumns()));
    }

    public Hbl checkAllContainerAssigned(ShipmentDetails shipment, Set<Containers> containersList, List<Packing> packings) {
        var shipmentId = shipment.getId();
        boolean allContainerAssigned = getAllContainerAssigned(containersList);
        Hbl hbl = null;
        if(allContainerAssigned) {
            List<Hbl> hbls = hblDao.findByShipmentId(shipmentId);
            if(!hbls.isEmpty()) {
                hbl = hbls.get(0);
                hbl = getHblWithContainerAndCargoData(shipment, containersList, packings, hbl);
            }
            else {
                ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
                if(shipmentSettingsDetails.getRestrictHblGen() == null || !shipmentSettingsDetails.getRestrictHblGen()) {
                    try {
                        hbl = getHblFromShipmentId(shipmentId);
                    } catch (Exception ex) {
                        log.error("HBL generation from Containers addition failed: " + ex.getMessage());
                    }
                }
            }
        }
        return hbl;
    }

    private boolean getAllContainerAssigned(Set<Containers> containersList) {
        boolean allContainerAssigned = true;
        for(Containers container: containersList) {
            if(container.getContainerNumber() == null || container.getContainerNumber().isEmpty()) {
                allContainerAssigned = false;
                break;
            }
        }
        return allContainerAssigned;
    }

    private Hbl getHblWithContainerAndCargoData(ShipmentDetails shipment, Set<Containers> containersList, List<Packing> packings, Hbl hbl) {
        boolean isContainerWithoutNumberOrNoContainer = false;
        if(hbl.getHblContainer() != null && !hbl.getHblContainer().isEmpty()) {
            for(HblContainerDto hblContainerDto: hbl.getHblContainer()) {
                if(hblContainerDto.getContainerNumber() == null || hblContainerDto.getContainerNumber().isEmpty()) {
                    isContainerWithoutNumberOrNoContainer = true;
                    break;
                }
            }
        }
        else
            isContainerWithoutNumberOrNoContainer = true;
        if (isContainerWithoutNumberOrNoContainer) {
            hbl.setHblContainer(mapShipmentContainersToHBL(shipment));
            hbl.setHblCargo(mapShipmentCargoToHBL(packings, containersList));
            hbl = hblDao.save(hbl);
        }
        return hbl;
    }

    private Hbl getHblFromShipmentId(Long shipmentId) throws RunnerException {
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(shipmentId);
        if (shipmentDetails.isEmpty())
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        this. validateBeforeGeneration(shipmentDetails.get());

        List<Hbl> hbls = hblDao.findByShipmentId(shipmentId);
        if (! hbls.isEmpty())
            throw new ValidationException(String.format(HblConstants.HBL_DATA_FOUND, shipmentDetails.get().getShipmentId()));

        Hbl hbl = getDefaultHblFromShipment(shipmentDetails.get());
        
        hbl = hblDao.save(hbl);
        return hbl;
    }
    private void validateBeforeGeneration(ShipmentDetails shipmentDetails){
        validateContainerNumberForContainers(shipmentDetails);
        if(!shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)
                || !shipmentDetails.getDirection().equals(Constants.DIRECTION_EXP)
                || (!shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) && !shipmentDetails.getShipmentType().equals(Constants.SHIPMENT_TYPE_LCL))
                || (shipmentDetails.getShipmentType().equals(Constants.SHIPMENT_TYPE_LCL) && Objects.equals(shipmentDetails.getJobType(), Constants.JOB_TYPE_CLB))
                || (shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) && !Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT))){
            return;
        }
        validateContainerNumberForPacks(shipmentDetails);
        if(shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) && Boolean.TRUE.equals(shipmentDetails.getContainsHazardous())) {
            if(!OceanDGStatus.OCEAN_DG_ACCEPTED.equals(shipmentDetails.getOceanDGStatus()) && !OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED.equals(shipmentDetails.getOceanDGStatus()) &&
                    !Constants.IMP.equals(shipmentDetails.getDirection()))
                throw new ValidationException("The shipment is marked as DG but is not approved. Please get the required DG approvals before generating Hbl.");
            if(shipmentDetails.getContainersList() == null ||
                    shipmentDetails.getContainersList().stream().filter(c -> Boolean.TRUE.equals(c.getHazardous())).toList().isEmpty())
                throw new ValidationException("The shipment is marked as DG but does not contain any DG containers. Please add DG containers before generating Hbl.");
        }
    }

    private void validateContainerNumberForPacks(ShipmentDetails shipmentDetails) {
        if(!Objects.isNull(shipmentDetails.getPackingList())
            && !Objects.equals(shipmentDetails.getShipmentType(), Constants.SHIPMENT_TYPE_LCL)) {
            var packsList = shipmentDetails.getPackingList().stream().filter(x -> Objects.isNull(x.getContainerId())).toList();
            if(!packsList.isEmpty()){
                throw new ValidationException("Container Number is Mandatory for HBL Generation, please assign the container number for all the packages in the shipment.");
            }
        }
    }

    private void validateContainerNumberForContainers(ShipmentDetails shipmentDetails) {
        if(!Objects.isNull(shipmentDetails.getContainersList())
            && !(Objects.equals(shipmentDetails.getShipmentType(), Constants.SHIPMENT_TYPE_LCL) || (Objects.equals(shipmentDetails.getShipmentType(), Constants.CARGO_TYPE_FCL) && !Objects.equals(shipmentDetails.getJobType(), Constants.SHIPMENT_TYPE_DRT)))) {
            List<Containers> containers = shipmentDetails.getContainersList().stream().filter(c -> StringUtility.isEmpty(c.getContainerNumber())).toList();
            if (!containers.isEmpty())
                throw new ValidationException("Please assign container number to all the containers before generating the HBL.");
        }
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> generateHBL(CommonRequestModel commonRequestModel) throws RunnerException {
        HblGenerateRequest request = (HblGenerateRequest) commonRequestModel.getData();
        Hbl hbl = getHblFromShipmentId(request.getShipmentId());

        hblSync(hbl);

        return ResponseHelper.buildSuccessResponse(convertEntityToDto(hbl));
    }
    @Override
    public ResponseEntity<IRunnerResponse> partialUpdateHBL(CommonRequestModel commonRequestModel) throws RunnerException {
        HblGenerateRequest request = (HblGenerateRequest) commonRequestModel.getData();
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(request.getShipmentId());
        if (shipmentDetails.isEmpty())
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);

        List<Hbl> hbls = hblDao.findByShipmentId(request.getShipmentId());
        if (hbls.isEmpty())
            throw new ValidationException(String.format(HblConstants.HBL_NO_DATA_FOUND_SHIPMENT, shipmentDetails.get().getShipmentId()));

        Hbl hbl = hbls.get(0);
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        if(shipmentSettingsDetails.getRestrictBLEdit() != null && shipmentSettingsDetails.getRestrictBLEdit()) {
            HblResetRequest resetRequest = HblResetRequest.builder().id(hbl.getId()).resetType(HblReset.ALL).build();
            return self.resetHbl(CommonRequestModel.buildRequest(resetRequest));
        } else if (shipmentSettingsDetails.getAutoUpdateShipmentBL() != null && shipmentSettingsDetails.getAutoUpdateShipmentBL()){
            updateHblFromShipment(shipmentDetails.get(), hbl, shipmentSettingsDetails);
            hbl = hblDao.save(hbl);
        }

        hblSync(hbl);

        return ResponseHelper.buildSuccessResponse(convertEntityToDto(hbl));
    }

    private void updateHblFromShipment(ShipmentDetails shipmentDetails, Hbl hbl, ShipmentSettingsDetails shipmentSettingsDetails) {
        updateShipmentToHBL(shipmentDetails, hbl, shipmentSettingsDetails.getHblLockSettings());
        updateShipmentCargoToHBL(shipmentDetails.getPackingList(), hbl, shipmentSettingsDetails.getHblLockSettings(), shipmentDetails.getContainersList());
        updateShipmentContainersToHBL(shipmentDetails.getContainersList(), hbl, shipmentSettingsDetails.getHblLockSettings());
        updateShipmentPartiesToHBL(shipmentDetails.getAdditionalDetails() != null ? shipmentDetails.getAdditionalDetails().getNotifyParty() : null, hbl, shipmentSettingsDetails.getHblLockSettings());

    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveByShipmentId(CommonRequestModel request) {
        Long shipmentId = ((CommonGetRequest) request.getData()).getId();

        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        if(shipmentSettingsDetails != null && ((shipmentSettingsDetails.getRestrictBLEdit() != null && shipmentSettingsDetails.getRestrictBLEdit()) || (shipmentSettingsDetails.getAutoUpdateShipmentBL() != null && shipmentSettingsDetails.getAutoUpdateShipmentBL()))){
            HblGenerateRequest req = HblGenerateRequest.builder().shipmentId(shipmentId).build();
            try {
                partialUpdateHBL(CommonRequestModel.buildRequest(req));
            } catch (Exception ex){
                log.error("Error while partially updating Hbl fields due to: "+ ex.getMessage());
            }
        }
        List<Hbl> hbls = hblDao.findByShipmentId(shipmentId);
        return ResponseHelper.buildSuccessResponse(hbls.isEmpty() ? null : convertEntityToDto(hbls.get(0)));
    }

    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> resetHbl(CommonRequestModel commonRequestModel) throws RunnerException {
        HblResetRequest request = (HblResetRequest) commonRequestModel.getData();
        Optional<Hbl> hblOptional = hblDao.findById(request.getId());
        if (hblOptional.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        Hbl hbl = hblOptional.get();
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(hbl.getShipmentId());
        if (shipmentDetails.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        switch (request.getResetType()) {

            case HBL_DATA:
                hbl.setHblData(mapShipmentToHBL(shipmentDetails.get()));
                break;

            case HBL_CARGOES:
                hbl.setHblCargo(mapShipmentCargoToHBL(shipmentDetails.get().getPackingList(), shipmentDetails.get().getContainersList()));
                break;

            case HBL_CONTAINERS:
                hbl.setHblContainer(mapShipmentContainersToHBL(shipmentDetails.get()));
                break;

            case HBL_PARTIES:
                hbl.setHblNotifyParty(mapShipmentPartiesToHBL(shipmentDetails.get().getAdditionalDetails() != null ? shipmentDetails.get().getAdditionalDetails().getNotifyParty() : null));
                break;

            case ALL:
                Hbl newHbl = getDefaultHblFromShipment(shipmentDetails.get());
                hbl.setHblData(newHbl.getHblData());
                hbl.setHblCargo(newHbl.getHblCargo());
                hbl.setHblContainer(newHbl.getHblContainer());
                hbl.setHblNotifyParty(newHbl.getHblNotifyParty());
                break;

        }
        hbl = hblDao.save(hbl);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(hbl));
    }

    private Hbl getDefaultHblFromShipment(ShipmentDetails shipmentDetails) throws RunnerException {
        HblDataDto hblData = mapShipmentToHBL(shipmentDetails);
        List<HblCargoDto> hblCargos = mapShipmentCargoToHBL(shipmentDetails.getPackingList(), shipmentDetails.getContainersList());
        List<HblContainerDto> hblContainers = mapShipmentContainersToHBL(shipmentDetails);
        List<HblPartyDto> hblParties = mapShipmentPartiesToHBL(shipmentDetails.getAdditionalDetails() != null ? shipmentDetails.getAdditionalDetails().getNotifyParty() : null);

        return Hbl.builder().shipmentId(shipmentDetails.getId())
                .hblData(hblData).hblCargo(hblCargos)
                .hblContainer(hblContainers).hblNotifyParty(hblParties)
                .build();
    }

    private Hbl convertRequestToEntity(HblRequest request) {
        if (Objects.isNull(request))
            return Hbl.builder().build();
        HblDataDto hblData = jsonHelper.convertValue(request, HblDataDto.class);
        return Hbl.builder().shipmentId(request.getShipmentId())
                .hblData(hblData).hblCargo(request.getCargoes())
                .hblContainer(request.getContainers())
                .hblNotifyParty(request.getNotifyParties())
                .build();
    }

    private IRunnerResponse convertEntityToDto(Hbl hbl) {
        HblResponse response = jsonHelper.convertValue(hbl.getHblData(), HblResponse.class);
        response.setCargoes(hbl.getHblCargo());
        response.setContainers(hbl.getHblContainer());
        response.setNotifyParties(hbl.getHblNotifyParty());
        response.setId(hbl.getId());
        response.setGuid(hbl.getGuid());
        return response;
    }


    private HblDataDto mapShipmentToHBL(ShipmentDetails shipmentDetail) throws RunnerException {
        HblDataDto hblData = HblDataDto.builder().build();
        hblData.setShipmentId(shipmentDetail.getId());
        Routings routing = getRoutingsForShipment(shipmentDetail);

        mapConsignerConsigneeToHbl(shipmentDetail, hblData);
        AdditionalDetails additionalDetails = shipmentDetail.getAdditionalDetails() != null ? shipmentDetail.getAdditionalDetails() : new AdditionalDetails();
        CarrierDetails carrierDetails = shipmentDetail.getCarrierDetails() != null ? shipmentDetail.getCarrierDetails() : new CarrierDetails();

        Map<String, EntityTransferUnLocations> v1Data = getUnLocationsData(additionalDetails, carrierDetails);
        setUnLocationsData(v1Data, hblData, additionalDetails, carrierDetails, "All");
        hblData.setCargoDescription(shipmentDetail.getGoodsDescription());
        hblData.setMarksAndNumbers(shipmentDetail.getMarksNum());
        hblData.setPackageCount(shipmentDetail.getNoOfPacks());
        hblData.setPackageType(shipmentDetail.getPacksUnit());
        hblData.setReason(Constants.EMPTY_STRING);
        hblData.setBlType(HblConstants.ORIGINAL_HBL);
        hblData.setStatus(Constants.PENDING);
        hblData.setPurchaseOrderNumber(null);
        hblData.setBlReferenceNumber(shipmentDetail.getBookingReference());
        hblData.setCargoNetWeight(shipmentDetail.getNetWeight());
        hblData.setCargoGrossWeight(shipmentDetail.getWeight());
        hblData.setCargoGrossVolume(shipmentDetail.getVolume());
        hblData.setCargoNetWeightUnit(shipmentDetail.getNetWeightUnit());
        hblData.setCargoGrossWeightUnit(shipmentDetail.getWeightUnit());
        hblData.setCargoGrossVolumeUnit(shipmentDetail.getVolumeUnit());
        boolean syncShipment = getSyncShipment(shipmentDetail);
        hblData.setHouseBill(shipmentDetail.getHouseBill());
        mapVoyageVesselFromRouting(routing, hblData, carrierDetails);
        hblData.setNoOfCopies(StringUtility.convertToString(additionalDetails.getCopy()));
        hblData.setVersion(1);
        hblData.setOriginOfGoods(additionalDetails.getGoodsCO());
        List<ShipmentOrder> shipmentOrders = shipmentDetail.getShipmentOrders();
        if(shipmentOrders != null && !shipmentOrders.isEmpty()) {
            hblData.setPurchaseOrderNumber(shipmentOrders.stream().map(ShipmentOrder::getOrderNumber).filter(Objects::nonNull).collect(Collectors.joining(", ")));
        }
        mapDeliveryDataInHbl(additionalDetails, hblData);
        // LATER: This needs to re-visit after incorporating this setting in service
        if (/*Unico HBL*/true) {
            hblData.setTransportType(shipmentDetail.getTransportMode());
            hblData.setShipmentType(shipmentDetail.getDirection());
            hblData.setShippingTime(carrierDetails.getEtd() == null ? null : carrierDetails.getEtd().toLocalTime().toString());
            hblData.setEtd(carrierDetails.getEtd());
            hblData.setIncoTerms(shipmentDetail.getIncoterms());
            hblData.setFinalDestination(carrierDetails.getDestination());
            hblData.setQuantity(shipmentDetail.getInnerPacks());
            hblData.setQuantityCode(shipmentDetail.getInnerPackUnit());
            if(shipmentDetail.getElDetailsList() != null) {
                hblData.setElNumber(shipmentDetail.getElDetailsList().stream().map(ELDetails::getElNumber).collect(Collectors.joining(",")));
                hblData.setElDate(shipmentDetail.getElDetailsList().stream().map(c -> c.getCreatedAt().toString()).collect(Collectors.joining(",")));
            }
            if(shipmentDetail.getReferenceNumbersList() != null) {
                hblData.setInvoiceNumbers(shipmentDetail.getReferenceNumbersList().stream().filter(c -> Objects.equals(c.getType(), Constants.INVNO))
                        .map(ReferenceNumbers::getReferenceNumber).collect(Collectors.joining(",")));
                hblData.setLcNumber(shipmentDetail.getReferenceNumbersList().stream().filter(c -> Objects.equals(c.getType(), Constants.CON))
                        .map(ReferenceNumbers::getReferenceNumber).collect(Collectors.joining(",")));
            }

        }
        if (syncShipment) {
            try {
                shipmentSync.sync(shipmentDetail, null, null, StringUtility.convertToString(shipmentDetail.getGuid()), false);
            } catch (Exception e) {
                log.error("Error performing sync on shipment entity, {}", e.getMessage(), e);
            }
        }

        return hblData;
    }

    private void mapVoyageVesselFromRouting(Routings routing, HblDataDto hblData, CarrierDetails carrierDetails) {
        if (Objects.nonNull(routing)) {
            hblData.setVesselName(masterDataUtil.getVesselName(routing.getVesselName()));
            hblData.setVoyage(routing.getVoyage());
        } else {
            hblData.setVesselName(masterDataUtil.getVesselName(carrierDetails.getVessel()));
            hblData.setVoyage(carrierDetails.getVoyage());
        }
    }

    private boolean getSyncShipment(ShipmentDetails shipmentDetail) throws RunnerException {
        boolean syncShipment = false;
        // generate HouseBill
        if(StringUtility.isEmpty(shipmentDetail.getHouseBill())) {
            shipmentDetail.setHouseBill(shipmentService.generateCustomHouseBL(shipmentDetail));
            shipmentDao.save(shipmentDetail, false);
            syncShipment = true;
        }
        return syncShipment;
    }

    private Routings getRoutingsForShipment(ShipmentDetails shipmentDetail) {
        Routings routing = null;
        if (Objects.nonNull(shipmentDetail.getRoutingsList()))
            routing = shipmentDetail.getRoutingsList().stream().filter(x -> Boolean.TRUE.equals(x.getIsSelectedForDocument())).findFirst().orElse(null);
        return routing;
    }

    private void mapDeliveryDataInHbl(AdditionalDetails additionalDetails, HblDataDto hblData) {
        if (!Objects.isNull(additionalDetails.getImportBroker())) {
            Parties broker = additionalDetails.getImportBroker();
            if (!Objects.isNull(broker.getOrgData()) && broker.getOrgData().containsKey(PartiesConstants.FULLNAME))
                hblData.setDeliveryAgent(String.valueOf(broker.getOrgData().get(PartiesConstants.FULLNAME)));
            if (!Objects.isNull(broker.getAddressData()) )
                hblData.setDeliveryAgentAddress(AwbUtility.constructAddress(broker.getAddressData()));
        }
    }

    private void mapConsignerConsigneeToHbl(ShipmentDetails shipmentDetail, HblDataDto hblData) {
        if(shipmentDetail.getConsigner() != null) {
            if (shipmentDetail.getConsigner().getOrgData() != null)
                hblData.setConsignorName(StringUtility.convertToString(shipmentDetail.getConsigner().getOrgData().get(PartiesConstants.FULLNAME)) );
            if (shipmentDetail.getConsigner().getAddressData() != null)
                hblData.setConsignorAddress(constructAddress(shipmentDetail.getConsigner().getAddressData()));
        }
        if(shipmentDetail.getConsignee() != null ) {
            if (shipmentDetail.getConsignee().getOrgData() != null)
                hblData.setConsigneeName(StringUtility.convertToString(shipmentDetail.getConsignee().getOrgData().get(PartiesConstants.FULLNAME)));
            if (shipmentDetail.getConsignee().getAddressData() != null)
                hblData.setConsigneeAddress(constructAddress(shipmentDetail.getConsignee().getAddressData()));
        }
    }

    private String constructAddress(Map<String, Object> addressData) {
        StringBuilder sb = new StringBuilder();
        String newLine = "\r\n";
        if (addressData.containsKey(PartiesConstants.COMPANY_NAME))
            sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.COMPANY_NAME)));
        if (addressData.containsKey(PartiesConstants.ADDRESS1))
            sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.ADDRESS1)));
        if (addressData.containsKey(PartiesConstants.CITY))
            sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.CITY)));
        if (addressData.containsKey(PartiesConstants.COUNTRY))
            sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.COUNTRY)));
        if (addressData.containsKey(PartiesConstants.ZIP_POST_CODE))
            sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.ZIP_POST_CODE)));
        if (addressData.containsKey(PartiesConstants.CONTACT_PHONE))
            sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.CONTACT_PHONE)));

        return  !sb.isEmpty() && sb.toString().length() >= 2 ? sb.substring(2) : sb.toString();
    }

    private List<HblContainerDto> mapShipmentContainersToHBL(ShipmentDetails shipment) {
        if(shipment == null)
            return null;
        CompanySettingsResponse companySettingsResponse = v1Service.retrieveCompanySettings();
        var referenceNumber = getReferenceNumber(shipment);
        Long noOfPackage = 0L;
        String packsType = null;
        String weightUnit = null;
        String volumeUnit = null;
        BigDecimal volume = BigDecimal.valueOf(0);
        BigDecimal weight = BigDecimal.valueOf(0);
        if(shipment.getPackingList() != null && !shipment.getPackingList().isEmpty())
        {
            for(Packing packing: shipment.getPackingList())
            {
                if(packing.getPacks() != null)
                    noOfPackage += Long.parseLong(packing.getPacks());
                if(packing.getVolume() != null)
                    volume = volume.add(packing.getVolume());
                if(packing.getWeight() != null)
                    weight = weight.add(packing.getWeight());
            }
            packsType = getPacksType(shipment);
            weightUnit = getWeightUnit(shipment);
            volumeUnit = getVolumeUnit(shipment);
        }
        List<HblContainerDto> referenceNumber1 = getHblContainerDtos(shipment, referenceNumber, companySettingsResponse, noOfPackage, packsType, volume, weight, volumeUnit, weightUnit);
        if (referenceNumber1 != null) return referenceNumber1;

        return getHblContainerDtos(shipment);

    }

    private List<ReferenceNumbers> getReferenceNumber(ShipmentDetails shipment) {
        return (shipment.getReferenceNumbersList() != null && !shipment.getReferenceNumbersList().isEmpty()) ? shipment.getReferenceNumbersList().stream().filter(x -> Objects.equals(x.getType(), "Container")).toList() : null;
    }

    private String getPacksType(ShipmentDetails shipment) {
        String packsType;
        var packTypes = shipment.getPackingList().stream().map(Packing::getPacksType).distinct().toList();
        packsType = packTypes.size() == 1 ? packTypes.get(0) : "MultiPack";
        return packsType;
    }

    private String getWeightUnit(ShipmentDetails shipment) {
        String weightUnit;
        var weightTypes = shipment.getPackingList().stream().map(Packing::getWeightUnit).distinct().toList();
        weightUnit = weightTypes.size() == 1 ? weightTypes.get(0) : "KG";
        return weightUnit;
    }

    private String getVolumeUnit(ShipmentDetails shipment) {
        String volumeUnit;
        var volumeTypes = shipment.getPackingList().stream().map(Packing::getVolumeUnit).distinct().toList();
        volumeUnit = volumeTypes.size() == 1 ? volumeTypes.get(0) : "M3";
        return volumeUnit;
    }

    private List<HblContainerDto> getHblContainerDtos(ShipmentDetails shipment) {
        var containers = shipment.getContainersList();
        if(Objects.equals(containers, null)) {
            containers = new HashSet<>();
        }
        List<HblContainerDto> hblContainers = new ArrayList<>();
        containers.forEach(container -> {
            HblContainerDto hblContainer = HblContainerDto.builder().build();
            hblContainer.setGuid(container.getGuid());
            hblContainer.setCarrierSealNumber(container.getCarrierSealNumber());
            hblContainer.setSealNumber(container.getSealNumber());
            hblContainer.setNoOfPackages(isStringNullOrEmpty(container.getPacks()) ? null : Long.valueOf(container.getPacks()));
            hblContainer.setContainerGrossVolume(container.getGrossVolume());
            hblContainer.setContainerGrossVolumeUnit(container.getGrossVolumeUnit());
            hblContainer.setContainerGrossWeight(container.getGrossWeight());
            hblContainer.setContainerGrossWeightUnit(container.getGrossWeightUnit());
            hblContainer.setContainerNumber(container.getContainerNumber());
            hblContainer.setContainerType(container.getContainerCode());
            hblContainer.setShipperSealNumber(container.getShipperSealNumber());
            hblContainer.setCustomsSealNumber(container.getCustomsSealNumber());
            hblContainer.setContainerDesc(container.getDescriptionOfGoods());
            hblContainer.setQuantity(container.getContainerCount());
            hblContainers.add(hblContainer);
        });
        return hblContainers;
    }

    private List<HblContainerDto> getHblContainerDtos(ShipmentDetails shipment, List<ReferenceNumbers> referenceNumber, CompanySettingsResponse companySettingsResponse, Long noOfPackage, String packsType, BigDecimal volume, BigDecimal weight, String volumeUnit, String weightUnit) {
        if(referenceNumber != null && !referenceNumber.isEmpty() && (shipment.getContainersList() == null || shipment.getContainersList().isEmpty()) && Objects.equals(shipment.getTransportMode(), "SEA") && Objects.equals(shipment.getShipmentType(), "LCL") && Boolean.TRUE.equals(companySettingsResponse.getSeaLclContainerFlag()))
        {
            return List.of(HblContainerDto.builder().
                    containerNumber(referenceNumber.get(0).getReferenceNumber()).
                    containerCount(1L).
                    noOfPackages(noOfPackage).
                    packsType(packsType).
                    containerGrossVolume(volume).
                    containerGrossWeight(weight).
                    containerGrossVolumeUnit(volumeUnit).
                    containerGrossWeightUnit(weightUnit).
                    build());
        }
        return null;
    }

    private List<HblCargoDto> mapShipmentCargoToHBL(List<Packing> packings, Set<Containers> containers) {
        List<HblCargoDto> hblCargoes = new ArrayList<>();
        Map<Long, String> map = new HashMap<>();
        if(containers != null && !containers.isEmpty())
            map = containers.stream().filter(e -> !isStringNullOrEmpty(e.getContainerNumber())).collect(Collectors.toMap(Containers::getId, Containers::getContainerNumber));
        Map<Long, String> finalMap = map;
        if(Objects.equals(packings, null)) {
            packings = new ArrayList<>();
        }
        packings.forEach(pack -> {
            HblCargoDto cargo = HblCargoDto.builder().build();
            cargo.setGuid(pack.getGuid());
            if(pack.getContainerId() != null && finalMap.containsKey(pack.getContainerId()))
                cargo.setBlContainerContainerNumber(finalMap.get(pack.getContainerId()));
            cargo.setCargoDesc(pack.getGoodsDescription());
            cargo.setCargoGrossVolume(pack.getVolume());
            cargo.setCargoGrossVolumeUnit(pack.getVolumeUnit());
            cargo.setCargoGrossWeight(pack.getWeight());
            cargo.setCargoGrossWeightUnit(pack.getWeightUnit());
            cargo.setHsCode(pack.getHSCode());
            cargo.setHazmatDetails(pack.getHazardous());
            cargo.setMarksAndNumbers(pack.getMarksnNums());
            cargo.setPackageCount(Integer.parseInt(pack.getPacks() == null ? "0" : pack.getPacks()));
            cargo.setPackageType(pack.getPacksType());
            hblCargoes.add(cargo);
        });

        return hblCargoes;
    }

    private List<HblPartyDto> mapShipmentPartiesToHBL(Parties party) {
        List<HblPartyDto> hblParties = new ArrayList<>();
        HblPartyDto hblParty = HblPartyDto.builder().build();
        if (party != null) {
            hblParty.setIsShipmentCreated(true);
            hblParty.setName(StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME)));
            hblParty.setAddress(constructAddress(party.getAddressData()));
            hblParty.setEmail(StringUtility.convertToString(party.getOrgData().get(PartiesConstants.EMAIL)));
            hblParties.add(hblParty);
        }
        return hblParties;
    }

    @Override
    public ResponseEntity<IRunnerResponse> saveV1Hbl(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException {
        String responseMsg;
        HblRequestV2 request = (HblRequestV2) commonRequestModel.getData();

        if(request == null || request.getShipmentGuid() == null) {
            log.error("Request Id and Shipment Guid is null for Hbl update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        if (checkForSync && !Objects.isNull(syncConfig.IS_REVERSE_SYNC_ACTIVE) && !Boolean.TRUE.equals(syncConfig.IS_REVERSE_SYNC_ACTIVE)) {
            return ResponseHelper.buildSuccessResponse();
        }

        ListCommonRequest listCommonRequest = constructListCommonRequest("guid", request.getShipmentGuid(), "=");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
        Page<ShipmentDetails> shipmentDetails = shipmentDao.findAll(pair.getLeft(), pair.getRight());
        if(shipmentDetails.isEmpty())
        {
            log.error("Shipment with provided guid not found {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        try{
            List<Hbl> hblList = hblDao.findByShipmentId(shipmentDetails.get().toList().get(0).getId());
            HblRequest hblRequest = jsonHelper.convertValue(request, HblRequest.class);
            Hbl entity = convertRequestToEntity(hblRequest);
            entity.setShipmentId(shipmentDetails.get().toList().get(0).getId());
            entity.setGuid(request.getGuid());
            if(!hblList.isEmpty()) {
                entity.setId(hblList.get(0).getId());
                entity.setGuid(hblList.get(0).getGuid());
            }
            entity = hblDao.save(entity);
            return ResponseHelper.buildSuccessResponse(convertEntityToDto(entity));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new GenericException(e);
        }
        
    }

    public void validateHblContainerNumberCondition(Hbl hblObject){
        if(!Objects.isNull(hblObject.getHblContainer()) ) {
            List<HblContainerDto> hblContainers = hblObject.getHblContainer().stream().filter(c -> StringUtility.isEmpty(c.getContainerNumber())).toList();
            if (!hblContainers.isEmpty())
                throw new ValidationException("Please assign container number to all the containers in HBL before generating the HBL.");
        }

        if(!Objects.isNull(hblObject.getHblCargo()) ) {
            List<HblCargoDto> hblCargos = hblObject.getHblCargo().stream().filter(c -> StringUtility.isEmpty(c.getBlContainerContainerNumber())).toList();
            if (!hblCargos.isEmpty())
                throw new ValidationException("Container Number is Mandatory for HBL Generation, please assign the container number for all the HBLCargo in the shipment.");
        }
    }
    private void updateShipmentToHBL(ShipmentDetails shipmentDetail, Hbl hbl, HblLockSettings hblLock) {
        HblDataDto hblData = hbl.getHblData();
        Routings routing = getRoutingsForShipment(shipmentDetail);

        setConsignerConsigneeDataInHbl(shipmentDetail, hblLock, hblData);
//        hblData.setOriginOfGoods(shipmentDetail.goo); : Missing in shipments
        AdditionalDetails additionalDetails = shipmentDetail.getAdditionalDetails() != null ? shipmentDetail.getAdditionalDetails() : new AdditionalDetails();
        CarrierDetails carrierDetails = shipmentDetail.getCarrierDetails() != null ? shipmentDetail.getCarrierDetails() : new CarrierDetails();

        setUnLocationsDataInHblData(hblLock, hblData, additionalDetails, carrierDetails);
        if(!Boolean.TRUE.equals(hblLock.getCargoDescriptionLock()))
            hblData.setCargoDescription(shipmentDetail.getGoodsDescription());
        if(!Boolean.TRUE.equals(hblLock.getMarksAndNumbersLock()))
            hblData.setMarksAndNumbers(shipmentDetail.getMarksNum());
        if(!Boolean.TRUE.equals(hblLock.getPackageCountLock()))
            hblData.setPackageCount(shipmentDetail.getNoOfPacks());
        if(!Boolean.TRUE.equals(hblLock.getPackageTypeLock()))
            hblData.setPackageType(shipmentDetail.getPacksUnit());
        if(!Boolean.TRUE.equals(hblLock.getBlReferenceNumberLock()))
            hblData.setBlReferenceNumber(shipmentDetail.getBookingReference());
        if(!Boolean.TRUE.equals(hblLock.getCargoNetWeightLock()))
            hblData.setCargoNetWeight(shipmentDetail.getNetWeight());
        if(!Boolean.TRUE.equals(hblLock.getCargoGrossWeightLock()))
            hblData.setCargoGrossWeight(shipmentDetail.getWeight());
        if(!Boolean.TRUE.equals(hblLock.getCargoGrossVolumeLock()))
            hblData.setCargoGrossVolume(shipmentDetail.getVolume());
        if(!Boolean.TRUE.equals(hblLock.getCargoNetWeightUnitLock()))
            hblData.setCargoNetWeightUnit(shipmentDetail.getNetWeightUnit());
        if(!Boolean.TRUE.equals(hblLock.getCargoGrossWeightUnitLock()))
            hblData.setCargoGrossWeightUnit(shipmentDetail.getWeightUnit());
        if(!Boolean.TRUE.equals(hblLock.getCargoGrossVolumeUnitLock()))
            hblData.setCargoGrossVolumeUnit(shipmentDetail.getVolumeUnit());
        if(!Boolean.TRUE.equals(hblLock.getHouseBillLock()))
            hblData.setHouseBill(shipmentDetail.getHouseBill());
        setVesselNameInHblData(hblLock, routing, hblData, carrierDetails);
        setVoyageInHblData(hblLock, routing, hblData, carrierDetails);

        processUnicoHblData(shipmentDetail, hblLock, hblData, carrierDetails);

    }

    /*Unico HBL*/
    private void processUnicoHblData(ShipmentDetails shipmentDetail, HblLockSettings hblLock, HblDataDto hblData, CarrierDetails carrierDetails) {
        // LATER: This needs to re-visit after incorporating this setting in service
        if (!Boolean.TRUE.equals(hblLock.getTransportTypeLock()))
            hblData.setTransportType(shipmentDetail.getTransportMode());
        if(!Boolean.TRUE.equals(hblLock.getShipmentTypeLock()))
            hblData.setShipmentType(shipmentDetail.getDirection());
        if(!Boolean.TRUE.equals(hblLock.getShippingTime()))
            hblData.setShippingTime(carrierDetails.getEtd() == null ? null : carrierDetails.getEtd().toLocalTime().toString());
        if(!Boolean.TRUE.equals(hblLock.getEtd()))
            hblData.setEtd(carrierDetails.getEtd());
        if(!Boolean.TRUE.equals(hblLock.getIncoTerms()))
            hblData.setIncoTerms(shipmentDetail.getIncoterms());
        if(!Boolean.TRUE.equals(hblLock.getFinalDestination()))
            hblData.setFinalDestination(carrierDetails.getDestination());
        if(!Boolean.TRUE.equals(hblLock.getQuantity()))
            hblData.setQuantity(shipmentDetail.getInnerPacks());
        if(!Boolean.TRUE.equals(hblLock.getQuantityCode()))
            hblData.setQuantityCode(shipmentDetail.getInnerPackUnit());
        setElDetailsListInHbl(shipmentDetail, hblLock, hblData);
        processReferenceNumbersListInHbl(shipmentDetail, hblLock, hblData);

    }

    private void setConsignerConsigneeDataInHbl(ShipmentDetails shipmentDetail, HblLockSettings hblLock, HblDataDto hblData) {
        if(shipmentDetail.getConsigner() != null) {
            if(!Boolean.TRUE.equals(hblLock.getConsignorNameLock()))
                hblData.setConsignorName(StringUtility.convertToString(shipmentDetail.getConsigner().getOrgData().get(PartiesConstants.FULLNAME)) );
            if(!Boolean.TRUE.equals(hblLock.getConsignorAddressLock()))
                hblData.setConsignorAddress(constructAddress(shipmentDetail.getConsigner().getAddressData()));
        }
        if(shipmentDetail.getConsignee() != null) {
            if(!Boolean.TRUE.equals(hblLock.getConsigneeNameLock()))
                hblData.setConsigneeName(StringUtility.convertToString(shipmentDetail.getConsignee().getOrgData().get(PartiesConstants.FULLNAME)));
            if(!Boolean.TRUE.equals(hblLock.getConsigneeAddressLock()))
                hblData.setConsigneeAddress(constructAddress(shipmentDetail.getConsignee().getAddressData()));
        }
    }

    private void processReferenceNumbersListInHbl(ShipmentDetails shipmentDetail, HblLockSettings hblLock, HblDataDto hblData) {
        if(shipmentDetail.getReferenceNumbersList() != null) {
            if(!Boolean.TRUE.equals(hblLock.getInvoiceNumbers()))
                hblData.setInvoiceNumbers(String.join(",",
                    shipmentDetail.getReferenceNumbersList().stream().filter(c -> Objects.equals(c.getType(), Constants.INVNO))
                            .map(c -> c.getReferenceNumber()).collect(Collectors.toList())));
            if(!Boolean.TRUE.equals(hblLock.getLcNumber()))
                hblData.setLcNumber(String.join(",",
                    shipmentDetail.getReferenceNumbersList().stream().filter(c -> Objects.equals(c.getType(), Constants.CON))
                            .map(c -> c.getReferenceNumber()).collect(Collectors.toList())));
        }
    }

    private void setElDetailsListInHbl(ShipmentDetails shipmentDetail, HblLockSettings hblLock, HblDataDto hblData) {
        if(shipmentDetail.getElDetailsList() != null) {
            if(!Boolean.TRUE.equals(hblLock.getElNumber()))
                hblData.setElNumber(String.join(",",
                    shipmentDetail.getElDetailsList().stream().map(c -> c.getElNumber()).collect(Collectors.toList())));
            if(!Boolean.TRUE.equals((hblLock.getElDate())))
                hblData.setElDate(String.join(",",
                    shipmentDetail.getElDetailsList().stream().map(c -> c.getCreatedAt().toString()).collect(Collectors.toList())));
        }
    }

    private void setVoyageInHblData(HblLockSettings hblLock, Routings routing, HblDataDto hblData, CarrierDetails carrierDetails) {
        if(!Boolean.TRUE.equals(hblLock.getVoyageLock())) {
            if (Objects.nonNull(routing))
                hblData.setVoyage(routing.getVoyage());
            else
                hblData.setVoyage(carrierDetails.getVoyage());
        }
    }

    private void setVesselNameInHblData(HblLockSettings hblLock, Routings routing, HblDataDto hblData, CarrierDetails carrierDetails) {
        if(!Boolean.TRUE.equals(hblLock.getVesselNameLock())) {
            if (Objects.nonNull(routing))
                hblData.setVesselName(masterDataUtil.getVesselName(routing.getVesselName()));
            else
                hblData.setVesselName(masterDataUtil.getVesselName(carrierDetails.getVessel()));
        }
    }

    private void setUnLocationsDataInHblData(HblLockSettings hblLock, HblDataDto hblData, AdditionalDetails additionalDetails, CarrierDetails carrierDetails) {
        Map<String, EntityTransferUnLocations> v1Data = getUnLocationsData(additionalDetails, carrierDetails);
        if(!Boolean.TRUE.equals(hblLock.getPlaceOfReceiptLock()))
            setUnLocationsData(v1Data, hblData, additionalDetails, carrierDetails, "PlaceOfReceipt");
        if(!Boolean.TRUE.equals(hblLock.getPortOfLoadLock()))
            setUnLocationsData(v1Data, hblData, additionalDetails, carrierDetails, "PortOfLoad");
        if(!Boolean.TRUE.equals(hblLock.getPortOfDischargeLock()))
            setUnLocationsData(v1Data, hblData, additionalDetails, carrierDetails, "PortOfDischarge");
        if(!Boolean.TRUE.equals(hblLock.getPlaceOfDeliveryLock()))
            setUnLocationsData(v1Data, hblData, additionalDetails, carrierDetails, "PlaceOfDelivery");
    }

    private void updateShipmentCargoToHBL(List<Packing> packings, Hbl hbl, HblLockSettings hblLock, Set<Containers> containers) {
        Map<UUID, Packing> packMap = new HashMap<>();
        packings.forEach(pack -> packMap.put(pack.getGuid(), pack));
        List<HblCargoDto> deletedList = new ArrayList<>();
        Map<Long, String> map = new HashMap<>();
        if(containers != null && !containers.isEmpty())
            map = containers.stream().collect(Collectors.toMap(Containers::getId, Containers::getContainerNumber));
        Map<Long, String> finalMap = map;
        if(hbl.getHblCargo() != null && !hbl.getHblCargo().isEmpty()) {
            removeInvalidaEntriesFromHblCargo(hbl, hblLock, packMap, finalMap, deletedList);
        }

        packMap.forEach((guid, pack) -> {
            HblCargoDto cargo = HblCargoDto.builder().build();
            cargo.setGuid(pack.getGuid());
            if(pack.getContainerId() != null && finalMap.containsKey(pack.getContainerId()))
                cargo.setBlContainerContainerNumber(finalMap.get(pack.getContainerId()));
            cargo.setCargoDesc(pack.getGoodsDescription());
            cargo.setCargoGrossVolume(pack.getVolume());
            cargo.setCargoGrossVolumeUnit(pack.getVolumeUnit());
            cargo.setCargoGrossWeight(pack.getWeight());
            cargo.setCargoGrossWeightUnit(pack.getWeightUnit());
            cargo.setHsCode(pack.getHSCode());
            cargo.setHazmatDetails(pack.getHazardous());
            cargo.setMarksAndNumbers(pack.getMarksnNums());
            cargo.setPackageCount(Integer.parseInt(pack.getPacks() == null ? "0" : pack.getPacks()));
            cargo.setPackageType(pack.getPacksType());
            if(hbl.getHblCargo() == null){
                hbl.setHblCargo(new ArrayList<>());
            }
            hbl.getHblCargo().add(cargo);
        });
    }

    private void removeInvalidaEntriesFromHblCargo(Hbl hbl, HblLockSettings hblLock, Map<UUID, Packing> packMap, Map<Long, String> finalMap, List<HblCargoDto> deletedList) {
        hbl.getHblCargo().forEach(cargo -> {
            if (packMap.containsKey(cargo.getGuid())) {
                String containerNumber = null;
                Packing packing = packMap.get(cargo.getGuid());
                if (packing != null && packing.getContainerId() != null && finalMap.containsKey(packing.getContainerId()))
                    containerNumber = finalMap.get(packing.getContainerId());
                updateShipmentCargoFieldToHbl(packing, cargo, hblLock, containerNumber);
                packMap.remove(cargo.getGuid());
            } else {
                deletedList.add(cargo);
            }
        });
        hbl.getHblCargo().removeAll(deletedList);
    }

    private void updateShipmentCargoFieldToHbl(Packing pack, HblCargoDto cargo, HblLockSettings hblLock, String containerNumber) {
        if(!Boolean.TRUE.equals(hblLock.getBlContainerIdLock()))
            cargo.setBlContainerContainerNumber(containerNumber);
        if(!Boolean.TRUE.equals(hblLock.getCargoDescriptionLock()))
            cargo.setCargoDesc(pack.getGoodsDescription());
        if(!Boolean.TRUE.equals(hblLock.getCargoGrossVolumeLock()))
            cargo.setCargoGrossVolume(pack.getVolume());
        if(!Boolean.TRUE.equals(hblLock.getCargoGrossVolumeUnitLock()))
            cargo.setCargoGrossVolumeUnit(pack.getVolumeUnit());
        if(!Boolean.TRUE.equals(hblLock.getCargoGrossWeightLock()))
            cargo.setCargoGrossWeight(pack.getWeight());
        if(!Boolean.TRUE.equals(hblLock.getCargoGrossWeightUnitLock()))
            cargo.setCargoGrossWeightUnit(pack.getWeightUnit());
        if(!Boolean.TRUE.equals(hblLock.getHsCodeLock()))
            cargo.setHsCode(pack.getHSCode());
        if(!Boolean.TRUE.equals(hblLock.getHazmatDetailsLock()))
            cargo.setHazmatDetails(pack.getHazardous());
        if(!Boolean.TRUE.equals(hblLock.getMarksAndNumbersLock()))
            cargo.setMarksAndNumbers(pack.getMarksnNums());
        if(!Boolean.TRUE.equals(hblLock.getPackageCountLock()))
            cargo.setPackageCount(Integer.parseInt(pack.getPacks() == null ? "0" : pack.getPacks()));
        if(!Boolean.TRUE.equals(hblLock.getPackageTypeLock()))
            cargo.setPackageType(pack.getPacksType());
    }
    private void updateShipmentContainersToHBL(Set<Containers> containers, Hbl hbl, HblLockSettings hblLock) {
        Map<UUID, Containers> contMap = new HashMap<>();
        containers.forEach(cont -> contMap.put(cont.getGuid(), cont));
        List<HblContainerDto> deletedList = new ArrayList<>();
        if(hbl.getHblContainer() != null && !hbl.getHblContainer().isEmpty()) {
            hbl.getHblContainer().forEach(hblCont -> {
                if (contMap.containsKey(hblCont.getGuid())) {
                    updateShipmentContainersToHBL(contMap.get(hblCont.getGuid()), hblCont, hblLock);
                    contMap.remove(hblCont.getGuid());
                } else {
                    deletedList.add(hblCont);
                }
            });
            hbl.getHblContainer().removeAll(deletedList);
        }

        if(!contMap.isEmpty()) {
            contMap.forEach((guid, container) -> {
                HblContainerDto hblContainer = HblContainerDto.builder().build();
                hblContainer.setGuid(container.getGuid());
                hblContainer.setCarrierSealNumber(container.getCarrierSealNumber());
                hblContainer.setSealNumber(container.getSealNumber());
                hblContainer.setNoOfPackages(isStringNullOrEmpty(container.getPacks()) ? null : Long.valueOf(container.getPacks()));
                hblContainer.setContainerGrossVolume(container.getGrossVolume());
                hblContainer.setContainerGrossVolumeUnit(container.getGrossVolumeUnit());
                hblContainer.setContainerGrossWeight(container.getGrossWeight());
                hblContainer.setContainerGrossWeightUnit(container.getGrossWeightUnit());
                hblContainer.setContainerNumber(container.getContainerNumber());
                hblContainer.setContainerType(container.getContainerCode());
                hblContainer.setShipperSealNumber(container.getShipperSealNumber());
                hblContainer.setCustomsSealNumber(container.getCustomsSealNumber());
                hblContainer.setContainerDesc(container.getDescriptionOfGoods());
                hblContainer.setQuantity(container.getContainerCount());
                if(hbl.getHblContainer() == null){
                    hbl.setHblContainer(new ArrayList<>());
                }
                hbl.getHblContainer().add(hblContainer);
            });
        }

    }
    private void updateShipmentContainersToHBL(Containers container, HblContainerDto hblContainer, HblLockSettings hblLock) {
        if(!Boolean.TRUE.equals(hblLock.getCarrierSealNumberLock()))
            hblContainer.setCarrierSealNumber(container.getCarrierSealNumber());
        if(!Boolean.TRUE.equals(hblLock.getContainerGrossVolumeLock()))
            hblContainer.setContainerGrossVolume(container.getGrossVolume());
        if(!Boolean.TRUE.equals(hblLock.getContainerGrossVolumeUnitLock()))
            hblContainer.setContainerGrossVolumeUnit(container.getGrossVolumeUnit());
        if(!Boolean.TRUE.equals(hblLock.getContainerGrossWeightLock()))
            hblContainer.setContainerGrossWeight(container.getGrossWeight());
        if(!Boolean.TRUE.equals(hblLock.getContainerGrossWeightUnitLock()))
            hblContainer.setContainerGrossWeightUnit(container.getGrossWeightUnit());
        if(!Boolean.TRUE.equals(hblLock.getContainerNumberLock()))
            hblContainer.setContainerNumber(container.getContainerNumber());
        if(!Boolean.TRUE.equals(hblLock.getContainerTypeLock()))
            hblContainer.setContainerType(container.getContainerCode());
        if(!Boolean.TRUE.equals(hblLock.getShipperSealNumberLock()))
            hblContainer.setShipperSealNumber(container.getShipperSealNumber());
        if(!Boolean.TRUE.equals(hblLock.getContainerDescLock()))
            hblContainer.setContainerDesc(container.getDescriptionOfGoods());
        if(!Boolean.TRUE.equals(hblLock.getQuantity()))
            hblContainer.setQuantity(container.getContainerCount());
    }

    private void updateShipmentPartiesToHBL(Parties party, Hbl hbl, HblLockSettings hblLock) {
        boolean createNotifyParty = true;
        HblPartyDto deleteParty = new HblPartyDto();
        if(hbl.getHblNotifyParty() != null && !hbl.getHblNotifyParty().isEmpty()) {
            for (var hblParty : hbl.getHblNotifyParty()) {
                if (hblParty.getIsShipmentCreated() != null && hblParty.getIsShipmentCreated()) {
                    createNotifyParty = false;
                    deleteParty = getDeleteParty(party, hblLock, hblParty, deleteParty);
                }
            }
            hbl.getHblNotifyParty().remove(deleteParty);
        }

        HblPartyDto hblParty = HblPartyDto.builder().build();
        if (party != null && createNotifyParty) {
            hblParty.setIsShipmentCreated(true);
            hblParty.setName(StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME)));
            hblParty.setAddress(constructAddress(party.getAddressData()));
            hblParty.setEmail(StringUtility.convertToString(party.getOrgData().get(PartiesConstants.EMAIL)));
            if(hbl.getHblNotifyParty() == null){
                hbl.setHblNotifyParty(new ArrayList<>());
            }
            hbl.getHblNotifyParty().add(hblParty);
        }

    }

    private HblPartyDto getDeleteParty(Parties party, HblLockSettings hblLock, HblPartyDto hblParty, HblPartyDto deleteParty) {
        if (party != null) {
            if (!Boolean.TRUE.equals(hblLock.getNotifyPartyNameLock()))
                hblParty.setName(StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME)));
            if (!Boolean.TRUE.equals(hblLock.getNotifyPartyAddressLock()))
                hblParty.setAddress(constructAddress(party.getAddressData()));
            if (!Boolean.TRUE.equals(hblLock.getNotifyPartyEmailLock()))
                hblParty.setEmail(StringUtility.convertToString(party.getOrgData().get(PartiesConstants.EMAIL)));
        } else {
            deleteParty = hblParty;
        }
        return deleteParty;
    }

    private Map<String, EntityTransferUnLocations> getUnLocationsData(AdditionalDetails additionalDetails, CarrierDetails carrierDetails) {
        List<String> locCodes = new ArrayList<>();

        if (!Objects.isNull(carrierDetails)) {
            locCodes.add(carrierDetails.getOriginPort());
            locCodes.add(carrierDetails.getDestinationPort());
            locCodes.add(carrierDetails.getDestination());
        }
        if (!Objects.isNull(additionalDetails)) {
            locCodes.add(additionalDetails.getPlaceOfSupply());
        }

        return masterDataUtil.fetchInBulkUnlocations(locCodes.stream().filter(Objects::nonNull).collect(Collectors.toSet()), EntityTransferConstants.LOCATION_SERVICE_GUID);
    }

    private void setUnLocationsData(Map<String, EntityTransferUnLocations> v1Data, HblDataDto hblDataDto, AdditionalDetails additionalDetails, CarrierDetails carrierDetails, String onField) {
        switch (onField) {
            case "PortOfLoad":
                if (!Objects.isNull(carrierDetails))
                    hblDataDto.setPortOfLoad(getUnLocationsName(v1Data, carrierDetails.getOriginPort()));
                break;
            case "PortOfDischarge":
                if (!Objects.isNull(carrierDetails))
                    hblDataDto.setPortOfDischarge(getUnLocationsName(v1Data, carrierDetails.getDestinationPort()));
                break;
            case "PlaceOfDelivery":
                if (!Objects.isNull(carrierDetails))
                    hblDataDto.setPlaceOfDelivery(getUnLocationsName(v1Data, carrierDetails.getDestination()));
                break;
            case "PlaceOfReceipt":
                if (!Objects.isNull(additionalDetails))
                    hblDataDto.setPlaceOfReceipt(getUnLocationsName(v1Data, additionalDetails.getPlaceOfSupply()));
                break;
            case "All":
                if (!Objects.isNull(carrierDetails)) {
                    hblDataDto.setPortOfLoad(getUnLocationsName(v1Data, carrierDetails.getOriginPort()));
                    hblDataDto.setPortOfDischarge(getUnLocationsName(v1Data, carrierDetails.getDestinationPort()));
                    hblDataDto.setPlaceOfDelivery(getUnLocationsName(v1Data, carrierDetails.getDestination()));
                }
                if (!Objects.isNull(additionalDetails)) {
                    hblDataDto.setPlaceOfReceipt(getUnLocationsName(v1Data, additionalDetails.getPlaceOfSupply()));
                }
                break;
            default:
        }

    }

    private String getUnLocationsName(Map<String, EntityTransferUnLocations> v1Data, String key) {
        if (Objects.isNull(key) || !v1Data.containsKey(key))
            return Constants.EMPTY_STRING;

        return v1Data.get(key).getLocCode() + " " + v1Data.get(key).getNameWoDiacritics();
    }

}
