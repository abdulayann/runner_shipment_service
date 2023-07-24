package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.hbl.HblCargoDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.response.HblResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IHblService;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;


@Slf4j
@Service
public class HblService implements IHblService {

    @Autowired
    private IHblDao hblDao;
    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        return null;
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
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(hbl.get()));
    }

    @Override
    public ResponseEntity<?> generateHBL(CommonRequestModel commonRequestModel) {
        HblGenerateRequest request = (HblGenerateRequest) commonRequestModel.getData();
        Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(request.getShipmentId());
        if (shipmentDetails.isEmpty())
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);

        List<Hbl> hbls = hblDao.findByShipmentId(request.getShipmentId());
        if (! hbls.isEmpty())
            throw new ValidationException(String.format(HblConstants.HBL_DATA_FOUND, shipmentDetails.get().getShipmentId()));

        HblDataDto hblData = mapShipmentToHBL(shipmentDetails.get());
        List<HblCargoDto> hblCargos = mapShipmentCargoToHBL(shipmentDetails.get().getPackingList());
        List<HblContainerDto> hblContainers = mapShipmentContainersToHBL(shipmentDetails.get().getContainersList());
        List<HblPartyDto> hblParties = mapShipmentPartiesToHBL(shipmentDetails.get().getAdditionalDetails().getNotifyParty());

        Hbl hbl = Hbl.builder().shipmentId(request.getShipmentId())
                .hblData(hblData).hblCargo(hblCargos)
                .hblContainer(hblContainers).hblNotifyParty(hblParties)
                .build();

        hbl = hblDao.save(hbl);
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(hbl));
    }

    @Override
    public ResponseEntity<?> retrieveByShipmentId(CommonRequestModel request) {
        Long shipmentId = ((CommonGetRequest) request.getData()).getId();
        List<Hbl> hbls = hblDao.findByShipmentId(shipmentId);
        if (hbls.isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(hbls.get(0)));
    }

    private Hbl convertRequestToEntity(HblRequest request) {
        HblDataDto hblData = modelMapper.map(request, HblDataDto.class);
        Hbl hbl = Hbl.builder().shipmentId(request.getShipmentId())
                .hblData(hblData).hblCargo(request.getCargoes())
                .hblContainer(request.getContainers()).hblNotifyParty(request.getNotifyParties())
                .build();
        return hbl;
    }

    private IRunnerResponse convertEntityToDto(Hbl hbl) {
        HblResponse response = modelMapper.map(hbl.getHblData(), HblResponse.class);
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
        hblData.setConsignorName(StringUtility.convertToString(shipmentDetail.getConsigner().getOrgData().get(PartiesConstants.FULLNAME)) );
        hblData.setConsignorAddress(constructAddress(shipmentDetail.getConsigner().getAddressData()));
        hblData.setConsigneeName(StringUtility.convertToString(shipmentDetail.getConsignee().getOrgData().get(PartiesConstants.FULLNAME)));
        hblData.setConsigneeAddress(constructAddress(shipmentDetail.getConsignee().getAddressData()));
//        hblData.setOriginOfGoods(shipmentDetail.goo); : Missing in shipments
        hblData.setPlaceOfReceipt(StringUtility.convertToString(shipmentDetail.getAdditionalDetails().getPlaceOfSupply()));
        hblData.setPortOfLoad(shipmentDetail.getCarrierDetails().getOrigin());
        hblData.setPortOfDischarge(shipmentDetail.getCarrierDetails().getDestination());
//        hblData.setPlaceOfDelivery(StringUtility.convertToString(shipmentDetail.getAdditionalDetails().getDe));
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
        hblData.setHouseBill(shipmentDetail.getHouseBill());
        hblData.setVesselName(shipmentDetail.getCarrierDetails().getVessel());
        hblData.setNoOfCopies(StringUtility.convertToString(shipmentDetail.getAdditionalDetails().getCopy()));
        hblData.setVersion(1);
        // TODO: This needs to re-visit after incorporating this setting in service
        if (/*Unico HBL*/true) {
            hblData.setTransportType(shipmentDetail.getTransportMode());
            hblData.setShipmentType(shipmentDetail.getDirection());
            hblData.setShippingTime(shipmentDetail.getCarrierDetails().getEtd() == null ? null : shipmentDetail.getCarrierDetails().getEtd().toLocalTime().toString());
            hblData.setEtd(shipmentDetail.getCarrierDetails().getEtd());
            hblData.setIncoTerms(shipmentDetail.getIncoterms());
//            hblData.setIncoTermPlace(shipmentDetail.incotermsDesctiption);
            hblData.setFinalDestination(shipmentDetail.getCarrierDetails().getDestination());
            hblData.setQuantity(shipmentDetail.getInnerPacks());
            hblData.setQuantityCode(shipmentDetail.getInnerPackUnit());
            hblData.setElNumber(String.join(",",
                    shipmentDetail.getElDetailsList().stream().map(c -> c.getElNumber()).collect(Collectors.toList())));
            hblData.setElDate(String.join(",",
                    shipmentDetail.getElDetailsList().stream().map(c -> c.getCreatedAt().toString()).collect(Collectors.toList())));
            hblData.setInvoiceNumbers(String.join(",",
                    shipmentDetail.getReferenceNumbersList().stream().filter(c -> c.getType() == Constants.INVNO)
                            .map(c -> c.getReferenceNumber()).collect(Collectors.toList())));
            hblData.setLcNumber(String.join(",",
                    shipmentDetail.getReferenceNumbersList().stream().filter(c -> c.getType() == Constants.CON)
                            .map(c -> c.getReferenceNumber()).collect(Collectors.toList())));

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

    private List<HblContainerDto> mapShipmentContainersToHBL(List<Containers> containers) {
        List<HblContainerDto> hblContainers = new ArrayList<>();
        containers.forEach(container -> {
            HblContainerDto hblContainer = HblContainerDto.builder().build();
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
            hblContainers.add(hblContainer);
        });

        return hblContainers;

    }

    private List<HblCargoDto> mapShipmentCargoToHBL(List<Packing> packings) {
        List<HblCargoDto> hblCargoes = new ArrayList<>();
        packings.forEach(pack -> {
            HblCargoDto cargo = HblCargoDto.builder().build();
            cargo.setBlContainerContainerNumber(pack.getContainerNumber());
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
            hblParty.setName(StringUtility.convertToString(party.getOrgData().get(PartiesConstants.FULLNAME)));
            hblParty.setAddress(constructAddress(party.getAddressData()));
            hblParty.setEmail(StringUtility.convertToString(party.getOrgData().get(PartiesConstants.EMAIL)));
            hblParties.add(hblParty);
        }
        return hblParties;
    }
    
}
