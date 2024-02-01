package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
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
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IHblService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.interfaces.ISyncQueueService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.HblRequestV2;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IHblSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.AwbUtility;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
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
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
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
    @Lazy
    @Autowired
    private ISyncQueueService syncQueueService;
    @Autowired
    private SyncConfig syncConfig;
    @Autowired
    private MasterDataUtils masterDataUtil;

    @Autowired
    @Lazy
    IShipmentService shipmentService;
    @Autowired
    private IV1Service v1Service;

    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Value("${v1service.url.base}${v1service.url.hblSync}")
    private String HBL_V1_SYNC_URL;

    @Autowired
    private IHblSync hblSync;

    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        HblRequest request = null;
        request = (HblRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Hbl create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Hbl hbl = convertRequestToEntity(request);
        try {
            hbl = hblDao.save(hbl);
            try {
                hblSync.sync(hbl);
            }
            catch (Exception e) {
                log.error("Error performing sync on hbl entity, {}", e);
            }
            log.info("Hbl Details created successfully for Id {} with Request Id {}", hbl.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(hbl));
    }

    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
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
            try {
                hblSync.sync(hbl);
            }
            catch (Exception e) {
                log.error("Error performing sync on hbl entity, {}", e);
            }
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
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            long id = request.getId();
            Optional<Hbl> hbl = hblDao.findById(id);
            if (!hbl.isPresent()) {
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            hblDao.delete(hbl.get());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        Long id = ((CommonGetRequest)commonRequestModel.getData()).getId();
        Optional<Hbl> hbl = hblDao.findById(id);
        if (hbl.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
       CommonGetRequest request=((CommonGetRequest)commonRequestModel.getData());
        if(request.getIncludeColumns()==null||request.getIncludeColumns().size()==0)
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(hbl.get()));
        else
            return ResponseHelper.buildSuccessResponse(PartialFetchUtils.fetchPartialListData(convertEntityToDto(hbl.get()),request.getIncludeColumns()));
    }

    public Hbl checkAllContainerAssigned(ShipmentDetails shipment, List<Containers> containersList, List<Packing> packings) {
        var shipmentId = shipment.getId();
        boolean allContainerAssigned = true;
        Hbl hbl = null;
        for(Containers container: containersList) {
            if(container.getContainerNumber() == null || container.getContainerNumber().isEmpty()) {
                allContainerAssigned = false;
                break;
            }
        }
        if(allContainerAssigned) {
            List<Hbl> hbls = hblDao.findByShipmentId(shipmentId);
            if(hbls.size() > 0) {
                hbl = hbls.get(0);
                boolean isContainerWithoutNumberOrNoContainer = false;
                if(hbl.getHblContainer() != null && hbl.getHblContainer().size() > 0) {
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
            }
            else {
                List<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
                if(shipmentSettingsDetails.size() > 0 && (shipmentSettingsDetails.get(0).getRestrictHblGen() == null || !shipmentSettingsDetails.get(0).getRestrictHblGen())) {
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

    private Hbl getHblFromShipmentId(Long shipmentId) {
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
        if(!Objects.isNull(shipmentDetails.getContainersList())) {
            List<Containers> containers = shipmentDetails.getContainersList().stream().filter(c -> StringUtility.isEmpty(c.getContainerNumber())).toList();
            if (!containers.isEmpty())
                throw new ValidationException("Please assign container number to all the containers before generating the HBL.");
        }
        if(!shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)
                || !shipmentDetails.getDirection().equals(Constants.DIRECTION_EXP)
                || (!shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL) && !shipmentDetails.getShipmentType().equals(Constants.SHIPMENT_TYPE_LCL))
                || (shipmentDetails.getShipmentType().equals(Constants.SHIPMENT_TYPE_LCL) && Objects.equals(shipmentDetails.getJobType(), Constants.JOB_TYPE_CLB))){
            return;
        }
        if(!Objects.isNull(shipmentDetails.getPackingList())) {
            var packsList = shipmentDetails.getPackingList().stream().filter(x -> Objects.isNull(x.getContainerId())).toList();
            if(!packsList.isEmpty()){
                throw new ValidationException("Container Number is Mandatory for HBL Generation, please assign the container number for all the packages in the shipment.");
            }
        }
    }

    @Override
    public ResponseEntity<?> generateHBL(CommonRequestModel commonRequestModel) {
        HblGenerateRequest request = (HblGenerateRequest) commonRequestModel.getData();
        Hbl hbl = getHblFromShipmentId(request.getShipmentId());

        try {
            hblSync.sync(hbl);
        }
        catch (Exception e) {
            log.error("Error performing sync on hbl entity, {}", e);
        }

        return ResponseHelper.buildSuccessResponse(convertEntityToDto(hbl));
    }
    @Override
    public ResponseEntity<?> partialUpdateHBL(CommonRequestModel commonRequestModel) {
        HblGenerateRequest request = (HblGenerateRequest) commonRequestModel.getData();
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(request.getShipmentId());
        if (shipmentDetails.isEmpty())
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);

        List<Hbl> hbls = hblDao.findByShipmentId(request.getShipmentId());
        if (hbls.isEmpty())
            throw new ValidationException(String.format(HblConstants.HBL_NO_DATA_FOUND_SHIPMENT, shipmentDetails.get().getShipmentId()));

        Hbl hbl = hbls.get(0);
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant()));
        if(shipmentSettingsDetailsList.isEmpty()){
            log.error("Failed to fetch Shipment Settings Details");
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDetailsList.get(0);
        if(shipmentSettingsDetails.getRestrictBLEdit()) {
            HblResetRequest resetRequest = HblResetRequest.builder().id(hbl.getId()).resetType(HblReset.ALL).build();
            return resetHbl(CommonRequestModel.buildRequest(resetRequest));
        } else if (shipmentSettingsDetails.getAutoUpdateShipmentBL()){
            updateHblFromShipment(shipmentDetails.get(), hbl, shipmentSettingsDetails);
            hbl = hblDao.save(hbl);
        }

        try {
            hblSync.sync(hbl);
        }
        catch (Exception e) {
            log.error("Error performing sync on hbl entity, {}", e);
        }

        return ResponseHelper.buildSuccessResponse(convertEntityToDto(hbl));
    }

    private void updateHblFromShipment(ShipmentDetails shipmentDetails, Hbl hbl, ShipmentSettingsDetails shipmentSettingsDetails) {
        updateShipmentToHBL(shipmentDetails, hbl, shipmentSettingsDetails.getHblLockSettings());
        updateShipmentCargoToHBL(shipmentDetails.getPackingList(), hbl, shipmentSettingsDetails.getHblLockSettings(), shipmentDetails.getContainersList());
        updateShipmentContainersToHBL(shipmentDetails.getContainersList(), hbl, shipmentSettingsDetails.getHblLockSettings());
        updateShipmentPartiesToHBL(shipmentDetails.getAdditionalDetails() != null ? shipmentDetails.getAdditionalDetails().getNotifyParty() : null, hbl, shipmentSettingsDetails.getHblLockSettings());

    }

    @Override
    public ResponseEntity<?> retrieveByShipmentId(CommonRequestModel request) {
        Long shipmentId = ((CommonGetRequest) request.getData()).getId();
        List<Hbl> hbls = hblDao.findByShipmentId(shipmentId);
        return ResponseHelper.buildSuccessResponse(hbls.isEmpty() ? null : convertEntityToDto(hbls.get(0)));
    }

    @Override
    public ResponseEntity<?> resetHbl(CommonRequestModel commonRequestModel) {
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

    private Hbl getDefaultHblFromShipment(ShipmentDetails shipmentDetails) {
        HblDataDto hblData = mapShipmentToHBL(shipmentDetails);
        List<HblCargoDto> hblCargos = mapShipmentCargoToHBL(shipmentDetails.getPackingList(), shipmentDetails.getContainersList());
        List<HblContainerDto> hblContainers = mapShipmentContainersToHBL(shipmentDetails);
        List<HblPartyDto> hblParties = mapShipmentPartiesToHBL(shipmentDetails.getAdditionalDetails() != null ? shipmentDetails.getAdditionalDetails().getNotifyParty() : null);

        Hbl hbl = Hbl.builder().shipmentId(shipmentDetails.getId())
                .hblData(hblData).hblCargo(hblCargos)
                .hblContainer(hblContainers).hblNotifyParty(hblParties)
                .build();

        return hbl;
    }

    private Hbl convertRequestToEntity(HblRequest request) {
        HblDataDto hblData = jsonHelper.convertValue(request, HblDataDto.class);
        Hbl hbl = Hbl.builder().shipmentId(request.getShipmentId())
                .hblData(hblData).hblCargo(request.getCargoes())
                .hblContainer(request.getContainers())
                .hblNotifyParty(request.getNotifyParties())
                .build();
        return hbl;
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


    private HblDataDto mapShipmentToHBL(ShipmentDetails shipmentDetail) {
        HblDataDto hblData = HblDataDto.builder().build();
        hblData.setShipmentId(shipmentDetail.getId());
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
        AdditionalDetails additionalDetails = shipmentDetail.getAdditionalDetails() != null ? shipmentDetail.getAdditionalDetails() : new AdditionalDetails();
        CarrierDetails carrierDetails = shipmentDetail.getCarrierDetails() != null ? shipmentDetail.getCarrierDetails() : new CarrierDetails();

        Map<String, EntityTransferUnLocations> v1Data = getUnLocationsData(hblData, additionalDetails, carrierDetails);
        setUnLocationsData(v1Data, hblData, additionalDetails, carrierDetails, "All");
        hblData.setCargoDescription(shipmentDetail.getGoodsDescription());
        hblData.setMarksAndNumbers(shipmentDetail.getMarksNum());
        hblData.setPackageCount(shipmentDetail.getNoOfPacks());
        hblData.setPackageType(shipmentDetail.getPacksUnit());
        hblData.setReason(StringUtility.getEmptyString());
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
        boolean syncShipment = false;
        // generate HouseBill
        if(StringUtility.isEmpty(shipmentDetail.getHouseBill())) {
            shipmentDetail.setHouseBill(shipmentService.generateCustomHouseBL(shipmentDetail));
            shipmentDao.save(shipmentDetail, false);
            syncShipment = true;
        }
        hblData.setHouseBill(shipmentDetail.getHouseBill());
        hblData.setVesselName(carrierDetails.getVessel());
        hblData.setNoOfCopies(StringUtility.convertToString(additionalDetails.getCopy()));
        hblData.setVersion(1);
        hblData.setOriginOfGoods(additionalDetails.getGoodsCO());
        hblData.setVoyage(carrierDetails.getVoyage());
        if (!Objects.isNull(additionalDetails.getExportBroker())) {
            Parties exportBroker = additionalDetails.getExportBroker();
            if (!Objects.isNull(exportBroker.getOrgData()) && exportBroker.getOrgData().containsKey(PartiesConstants.FULLNAME))
                hblData.setDeliveryAgent(String.valueOf(exportBroker.getOrgData().get(PartiesConstants.FULLNAME)));
            if (!Objects.isNull(exportBroker.getAddressData()) )
                hblData.setDeliveryAgentAddress(AwbUtility.constructAddress(exportBroker.getAddressData()));
        }
        UnlocationsResponse destination = ReportHelper.getUNLocRow(carrierDetails.getDestination());
        // TODO: This needs to re-visit after incorporating this setting in service
        if (/*Unico HBL*/true) {
            hblData.setTransportType(shipmentDetail.getTransportMode());
            hblData.setShipmentType(shipmentDetail.getDirection());
            hblData.setShippingTime(carrierDetails.getEtd() == null ? null : carrierDetails.getEtd().toLocalTime().toString());
            hblData.setEtd(carrierDetails.getEtd());
            hblData.setIncoTerms(shipmentDetail.getIncoterms());
//            hblData.setIncoTermPlace(shipmentDetail.incotermsDesctiption);
            hblData.setFinalDestination(carrierDetails.getDestination());
            hblData.setQuantity(shipmentDetail.getInnerPacks());
            hblData.setQuantityCode(shipmentDetail.getInnerPackUnit());
            if(shipmentDetail.getElDetailsList() != null) {
                hblData.setElNumber(String.join(",",
                        shipmentDetail.getElDetailsList().stream().map(c -> c.getElNumber()).collect(Collectors.toList())));
                hblData.setElDate(String.join(",",
                        shipmentDetail.getElDetailsList().stream().map(c -> c.getCreatedAt().toString()).collect(Collectors.toList())));
            }
            if(shipmentDetail.getReferenceNumbersList() != null) {
                hblData.setInvoiceNumbers(String.join(",",
                        shipmentDetail.getReferenceNumbersList().stream().filter(c -> c.getType() == Constants.INVNO)
                                .map(c -> c.getReferenceNumber()).collect(Collectors.toList())));
                hblData.setLcNumber(String.join(",",
                        shipmentDetail.getReferenceNumbersList().stream().filter(c -> c.getType() == Constants.CON)
                                .map(c -> c.getReferenceNumber()).collect(Collectors.toList())));
            }

        }
        if (syncShipment) {
            try {
                shipmentSync.sync(shipmentDetail, null, null);
            } catch (Exception e) {
                log.error("Error performing sync on shipment entity, {}", e);
            }
        }

        return hblData;
    }

    private String constructAddress(Map<String, Object> addressData) {
        StringBuilder sb = new StringBuilder();
        String newLine = "\r\n";
        if (addressData.containsKey(PartiesConstants.COMPANY_NAME))
            sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.COMPANY_NAME)));
        if (addressData.containsKey(PartiesConstants.CITY))
            sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.CITY)));
        if (addressData.containsKey(PartiesConstants.COUNTRY))
            sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.COUNTRY)));
        if (addressData.containsKey(PartiesConstants.PIN_CODE))
            sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.PIN_CODE)));
        if (addressData.containsKey(PartiesConstants.CONTACT_NUMBER))
            sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.CONTACT_NUMBER)));

        return sb.toString();
    }

    private List<HblContainerDto> mapShipmentContainersToHBL(ShipmentDetails shipment) {
        if(shipment == null)
            return null;
        CompanySettingsResponse companySettingsResponse = v1Service.retrieveCompanySettings();
        var referenceNumber = (shipment.getReferenceNumbersList() != null && shipment.getReferenceNumbersList().size() > 0) ? shipment.getReferenceNumbersList().stream().filter(x -> Objects.equals(x.getType(), "Container")).toList() : null;
        Long noOfPackage = 0L;
        String packsType = null;
        String weightUnit = null;
        String volumeUnit = null;
        BigDecimal volume = BigDecimal.valueOf(0);
        BigDecimal weight = BigDecimal.valueOf(0);
        if(shipment.getPackingList() != null && shipment.getPackingList().size() > 0)
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
            var packTypes = shipment.getPackingList().stream().map(Packing::getPacksType).distinct().toList();
            packsType = packTypes != null && packTypes.size() == 1 ? packTypes.get(0) : "MultiPack";
            var weightTypes = shipment.getPackingList().stream().map(Packing::getWeightUnit).distinct().toList();
            weightUnit = weightTypes != null && weightTypes.size() == 1 ? weightTypes.get(0) : "KG";
            var volumeTypes = shipment.getPackingList().stream().map(Packing::getVolumeUnit).distinct().toList();
            volumeUnit = volumeTypes != null && volumeTypes.size() == 1 ? volumeTypes.get(0) : "M3";
        }
        if(referenceNumber != null && referenceNumber.size() > 0 && (shipment.getContainersList() == null || shipment.getContainersList().isEmpty()) && Objects.equals(shipment.getTransportMode(), "SEA") && Objects.equals(shipment.getShipmentType(), "LCL") && companySettingsResponse.getSeaLclContainerFlag())
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
        var containers = shipment.getContainersList();
        List<HblContainerDto> hblContainers = new ArrayList<>();
        containers.forEach(container -> {
            HblContainerDto hblContainer = HblContainerDto.builder().build();
            hblContainer.setGuid(container.getGuid());
            hblContainer.setCarrierSealNumber(container.getCarrierSealNumber());
            hblContainer.setSealNumber(container.getSealNumber());
            hblContainer.setNoOfPackages(container.getNoOfPackages());
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

    private List<HblCargoDto> mapShipmentCargoToHBL(List<Packing> packings, List<Containers> containers) {
        List<HblCargoDto> hblCargoes = new ArrayList<>();
        Map<Long, String> map = new HashMap<>();
        if(containers != null && containers.size() > 0)
            map = containers.stream().collect(Collectors.toMap(Containers::getId, Containers::getContainerNumber));
        Map<Long, String> finalMap = map;
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
    public ResponseEntity<?> saveV1Hbl(CommonRequestModel commonRequestModel, boolean checkForSync) throws Exception {
        String responseMsg;
        HblRequestV2 request = (HblRequestV2) commonRequestModel.getData();
        if(request == null) {
            log.error("Request is empty for Hbl update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getShipmentGuid() == null) {
            log.error("Request Id and Shipment Guid is null for Hbl update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        if (checkForSync && !Objects.isNull(syncConfig.IS_REVERSE_SYNC_ACTIVE) && !syncConfig.IS_REVERSE_SYNC_ACTIVE) {
            return syncQueueService.saveSyncRequest(SyncingConstants.HBL, StringUtility.convertToString(request.getShipmentGuid()), request);
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
            List<Hbl> hblList = hblDao.findByShipmentId(shipmentDetails.get().collect(Collectors.toList()).get(0).getId());
            HblRequest hblRequest = jsonHelper.convertValue(request, HblRequest.class);
            Hbl entity = convertRequestToEntity(hblRequest);
            entity.setShipmentId(shipmentDetails.get().collect(Collectors.toList()).get(0).getId());
            if(!hblList.isEmpty() && hblList.size() > 0) {
                entity.setId(hblList.get(0).getId());
                entity.setGuid(hblList.get(0).getGuid());
            }
            entity = hblDao.save(entity);
            return ResponseHelper.buildSuccessResponse(convertEntityToDto(entity));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
        
    }
    private void updateShipmentToHBL(ShipmentDetails shipmentDetail, Hbl hbl, HblLockSettings hblLock) {
        HblDataDto hblData = hbl.getHblData();

        if(shipmentDetail.getConsigner() != null) {
            if(!hblLock.getConsignorNameLock())
                hblData.setConsignorName(StringUtility.convertToString(shipmentDetail.getConsigner().getOrgData().get(PartiesConstants.FULLNAME)) );
            if(!hblLock.getConsignorAddressLock())
                hblData.setConsignorAddress(constructAddress(shipmentDetail.getConsigner().getAddressData()));
        }
        if(shipmentDetail.getConsignee() != null) {
            if(!hblLock.getConsigneeNameLock())
                hblData.setConsigneeName(StringUtility.convertToString(shipmentDetail.getConsignee().getOrgData().get(PartiesConstants.FULLNAME)));
            if(!hblLock.getConsigneeAddressLock())
                hblData.setConsigneeAddress(constructAddress(shipmentDetail.getConsignee().getAddressData()));
        }
//        hblData.setOriginOfGoods(shipmentDetail.goo); : Missing in shipments
        AdditionalDetails additionalDetails = shipmentDetail.getAdditionalDetails() != null ? shipmentDetail.getAdditionalDetails() : new AdditionalDetails();
        CarrierDetails carrierDetails = shipmentDetail.getCarrierDetails() != null ? shipmentDetail.getCarrierDetails() : new CarrierDetails();

        Map<String, EntityTransferUnLocations> v1Data = getUnLocationsData(hblData, additionalDetails, carrierDetails);
        if(!hblLock.getPlaceOfReceiptLock())
            setUnLocationsData(v1Data, hblData, additionalDetails, carrierDetails, "PlaceOfReceipt");
        if(!hblLock.getPortOfLoadLock())
            setUnLocationsData(v1Data, hblData, additionalDetails, carrierDetails, "PortOfLoad");
        if(!hblLock.getPortOfDischargeLock())
            setUnLocationsData(v1Data, hblData, additionalDetails, carrierDetails, "PortOfDischarge");
        if(!hblLock.getPlaceOfDeliveryLock())
            setUnLocationsData(v1Data, hblData, additionalDetails, carrierDetails, "PlaceOfDelivery");
        if(!hblLock.getCargoDescriptionLock())
            hblData.setCargoDescription(shipmentDetail.getGoodsDescription());
        if(!hblLock.getMarksAndNumbersLock())
            hblData.setMarksAndNumbers(shipmentDetail.getMarksNum());
        if(!hblLock.getPackageCountLock())
            hblData.setPackageCount(shipmentDetail.getNoOfPacks());
        if(!hblLock.getPackageTypeLock())
            hblData.setPackageType(shipmentDetail.getPacksUnit());
        if(!hblLock.getBlReferenceNumberLock())
            hblData.setBlReferenceNumber(shipmentDetail.getBookingReference());
        if(!hblLock.getCargoNetWeightLock())
            hblData.setCargoNetWeight(shipmentDetail.getNetWeight());
        if(!hblLock.getCargoGrossWeightLock())
            hblData.setCargoGrossWeight(shipmentDetail.getWeight());
        if(!hblLock.getCargoGrossVolumeLock())
            hblData.setCargoGrossVolume(shipmentDetail.getVolume());
        if(!hblLock.getCargoNetWeightUnitLock())
            hblData.setCargoNetWeightUnit(shipmentDetail.getNetWeightUnit());
        if(!hblLock.getCargoGrossWeightUnitLock())
            hblData.setCargoGrossWeightUnit(shipmentDetail.getWeightUnit());
        if(!hblLock.getCargoGrossVolumeUnitLock())
            hblData.setCargoGrossVolumeUnit(shipmentDetail.getVolumeUnit());
        if(!hblLock.getHouseBillLock())
            hblData.setHouseBill(shipmentDetail.getHouseBill());
        if(!hblLock.getVesselNameLock())
            hblData.setVesselName(carrierDetails.getVessel());

        // TODO: This needs to re-visit after incorporating this setting in service
        if (/*Unico HBL*/true) {
            if(!hblLock.getTransportTypeLock())
                hblData.setTransportType(shipmentDetail.getTransportMode());
            if(!hblLock.getShipmentTypeLock())
                hblData.setShipmentType(shipmentDetail.getDirection());
            if(!hblLock.getShippingTime())
                hblData.setShippingTime(carrierDetails.getEtd() == null ? null : carrierDetails.getEtd().toLocalTime().toString());
            if(!hblLock.getEtd())
                hblData.setEtd(carrierDetails.getEtd());
            if(!hblLock.getIncoTerms())
                hblData.setIncoTerms(shipmentDetail.getIncoterms());
//            hblData.setIncoTermPlace(shipmentDetail.incotermsDesctiption);
            if(!hblLock.getFinalDestination())
                hblData.setFinalDestination(carrierDetails.getDestination());
            if(!hblLock.getQuantity())
                hblData.setQuantity(shipmentDetail.getInnerPacks());
            if(!hblLock.getQuantityCode())
                hblData.setQuantityCode(shipmentDetail.getInnerPackUnit());
            if(shipmentDetail.getElDetailsList() != null) {
                if(!hblLock.getElNumber())
                    hblData.setElNumber(String.join(",",
                        shipmentDetail.getElDetailsList().stream().map(c -> c.getElNumber()).collect(Collectors.toList())));
                if(!hblLock.getElDate())
                    hblData.setElDate(String.join(",",
                        shipmentDetail.getElDetailsList().stream().map(c -> c.getCreatedAt().toString()).collect(Collectors.toList())));
            }
            if(shipmentDetail.getReferenceNumbersList() != null) {
                if(!hblLock.getInvoiceNumbers())
                    hblData.setInvoiceNumbers(String.join(",",
                        shipmentDetail.getReferenceNumbersList().stream().filter(c -> c.getType() == Constants.INVNO)
                                .map(c -> c.getReferenceNumber()).collect(Collectors.toList())));
                if(!hblLock.getLcNumber())
                    hblData.setLcNumber(String.join(",",
                        shipmentDetail.getReferenceNumbersList().stream().filter(c -> c.getType() == Constants.CON)
                                .map(c -> c.getReferenceNumber()).collect(Collectors.toList())));
            }

        }

    }
    private void updateShipmentCargoToHBL(List<Packing> packings, Hbl hbl, HblLockSettings hblLock, List<Containers> containers) {
        Map<UUID, Packing> packMap = new HashMap<>();
        packings.forEach(pack -> {
            packMap.put(pack.getGuid(), pack);
        });
        List<HblCargoDto> deletedList = new ArrayList<>();
        Map<Long, String> map = new HashMap<>();
        if(containers != null && containers.size() > 0)
            map = containers.stream().collect(Collectors.toMap(Containers::getId, Containers::getContainerNumber));
        Map<Long, String> finalMap = map;
        hbl.getHblCargo().forEach(cargo -> {
            if(packMap.containsKey(cargo.getGuid())){
                String containerNumber = null;
                Packing packing = packMap.get(cargo.getGuid());
                if(packing != null && packing.getContainerId() != null && finalMap.containsKey(packing.getContainerId()))
                    containerNumber = finalMap.get(packing.getContainerId());
                updateShipmentCargoFieldToHbl(packing, cargo, hblLock, containerNumber);
                packMap.remove(cargo.getGuid());
            }else {
                deletedList.add(cargo);
            }
        });
        hbl.getHblCargo().removeAll(deletedList);

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
            hbl.getHblCargo().add(cargo);
        });
    }

    private void updateShipmentCargoFieldToHbl(Packing pack, HblCargoDto cargo, HblLockSettings hblLock, String containerNumber) {
        if(!hblLock.getBlContainerIdLock())
            cargo.setBlContainerContainerNumber(containerNumber);
        if(!hblLock.getCargoDescriptionLock())
            cargo.setCargoDesc(pack.getGoodsDescription());
        if(!hblLock.getCargoGrossVolumeLock())
            cargo.setCargoGrossVolume(pack.getVolume());
        if(!hblLock.getCargoGrossVolumeUnitLock())
            cargo.setCargoGrossVolumeUnit(pack.getVolumeUnit());
        if(!hblLock.getCargoGrossWeightLock())
            cargo.setCargoGrossWeight(pack.getWeight());
        if(!hblLock.getCargoGrossWeightUnitLock())
            cargo.setCargoGrossWeightUnit(pack.getWeightUnit());
        if(!hblLock.getHsCodeLock())
            cargo.setHsCode(pack.getHSCode());
        if(!hblLock.getHazmatDetailsLock())
            cargo.setHazmatDetails(pack.getHazardous());
        if(!hblLock.getMarksAndNumbersLock())
            cargo.setMarksAndNumbers(pack.getMarksnNums());
        if(!hblLock.getPackageCountLock())
            cargo.setPackageCount(Integer.parseInt(pack.getPacks() == null ? "0" : pack.getPacks()));
        if(!hblLock.getPackageTypeLock())
            cargo.setPackageType(pack.getPacksType());
    }
    private void updateShipmentContainersToHBL(List<Containers> containers, Hbl hbl, HblLockSettings hblLock) {
        Map<UUID, Containers> contMap = new HashMap<>();
        containers.forEach(cont -> {
            contMap.put(cont.getGuid(), cont);
        });
        List<HblContainerDto> deletedList = new ArrayList<>();
        hbl.getHblContainer().forEach(hblCont -> {
            if(contMap.containsKey(hblCont.getGuid())){
                updateShipmentContainersToHBL(contMap.get(hblCont.getGuid()), hblCont, hblLock);
                contMap.remove(hblCont.getGuid());
            }else {
                deletedList.add(hblCont);
            }
        });
        hbl.getHblContainer().removeAll(deletedList);

        if(!contMap.isEmpty()) {
            contMap.forEach((guid, container) -> {
                HblContainerDto hblContainer = HblContainerDto.builder().build();
                hblContainer.setGuid(container.getGuid());
                hblContainer.setCarrierSealNumber(container.getCarrierSealNumber());
                hblContainer.setSealNumber(container.getSealNumber());
                hblContainer.setNoOfPackages(container.getNoOfPackages());
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
                hbl.getHblContainer().add(hblContainer);
            });
        }

    }
    private void updateShipmentContainersToHBL(Containers container, HblContainerDto hblContainer, HblLockSettings hblLock) {
        if(!hblLock.getCarrierSealNumberLock())
            hblContainer.setCarrierSealNumber(container.getCarrierSealNumber());
        if(!hblLock.getContainerGrossVolumeLock())
            hblContainer.setContainerGrossVolume(container.getGrossVolume());
        if(!hblLock.getContainerGrossVolumeUnitLock())
            hblContainer.setContainerGrossVolumeUnit(container.getGrossVolumeUnit());
        if(!hblLock.getContainerGrossWeightLock())
            hblContainer.setContainerGrossWeight(container.getGrossWeight());
        if(!hblLock.getContainerGrossWeightUnitLock())
            hblContainer.setContainerGrossWeightUnit(container.getGrossWeightUnit());
        if(!hblLock.getContainerNumberLock())
            hblContainer.setContainerNumber(container.getContainerNumber());
        if(!hblLock.getContainerTypeLock())
            hblContainer.setContainerType(container.getContainerCode());
        if(!hblLock.getShipperSealNumberLock())
            hblContainer.setShipperSealNumber(container.getShipperSealNumber());
        if(!hblLock.getContainerDescLock())
            hblContainer.setContainerDesc(container.getDescriptionOfGoods());
        if(!hblLock.getQuantity())
            hblContainer.setQuantity(container.getContainerCount());
    }

    private void updateShipmentPartiesToHBL(Parties party, Hbl hbl, HblLockSettings hblLock) {
        boolean createNotifyParty = true;
        HblPartyDto deleteParty = new HblPartyDto();
        for(var hblParty: hbl.getHblNotifyParty()){
            if(hblParty.getIsShipmentCreated() != null && hblParty.getIsShipmentCreated()){
                createNotifyParty = false;
                if(party != null){
                    if(!hblLock.getNotifyPartyNameLock())
                        hblParty.setName(StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME)));
                    if(!hblLock.getNotifyPartyAddressLock())
                        hblParty.setAddress(constructAddress(party.getAddressData()));
                    if(!hblLock.getNotifyPartyEmailLock())
                        hblParty.setEmail(StringUtility.convertToString(party.getOrgData().get(PartiesConstants.EMAIL)));
                } else {
                    deleteParty = hblParty;
                }
            }
        }
        hbl.getHblNotifyParty().remove(deleteParty);

        HblPartyDto hblParty = HblPartyDto.builder().build();
        if (party != null && createNotifyParty) {
            hblParty.setIsShipmentCreated(true);
            hblParty.setName(StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME)));
            hblParty.setAddress(constructAddress(party.getAddressData()));
            hblParty.setEmail(StringUtility.convertToString(party.getOrgData().get(PartiesConstants.EMAIL)));
            hbl.getHblNotifyParty().add(hblParty);
        }

    }

    private Map<String, EntityTransferUnLocations> getUnLocationsData(HblDataDto hblDataDto, AdditionalDetails additionalDetails, CarrierDetails carrierDetails) {
        List<String> locCodes = new ArrayList<>();

        if (!Objects.isNull(carrierDetails)) {
            locCodes.add(carrierDetails.getOriginPort());
            locCodes.add(carrierDetails.getDestinationPort());
            locCodes.add(carrierDetails.getDestination());
        }
        if (!Objects.isNull(additionalDetails)) {
            locCodes.add(additionalDetails.getPlaceOfSupply());
        }

        Map<String, EntityTransferUnLocations> v1Data = masterDataUtil.fetchInBulkUnlocations(locCodes.stream().filter(Objects::nonNull).collect(Collectors.toList()), EntityTransferConstants.LOCATION_SERVICE_GUID);
        return v1Data;
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
        }

    }

    private String getUnLocationsName(Map<String, EntityTransferUnLocations> v1Data, String key) {
        if (Objects.isNull(key) || !v1Data.containsKey(key))
            return StringUtility.getEmptyString();

        return v1Data.get(key).getPortName();
    }
    
}
