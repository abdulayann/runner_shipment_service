package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.impl.ShipmentService;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.Entity.response.CustomShipmentSyncResponse;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.stream.Collectors;

@Component
@Slf4j
public class ShipmentSync implements IShipmentSync {

    @Autowired
    ModelMapper modelMapper;
    @Autowired
    JsonHelper jsonHelper;
    @Autowired
    RestTemplate restTemplate;
    @Autowired
    ShipmentService shipmentService;

    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();;

    @Value("${v1service.url.base}${v1service.url.shipmentSync}")
    private String SHIPMENT_V1_SYNC_URL;

    @Override
    public ResponseEntity<?> sync(ShipmentDetails sd) {
        CustomShipmentSyncRequest temp = new CustomShipmentSyncRequest();

        // First map nested entity that are root level properties in v1
        mapAdditionalDetails(temp, sd);
        mapCarrierDetails(temp, sd);
        // Map remaining object so there's no info lost for root -> root properties
        // example Guid
       CustomShipmentSyncRequest cs = modelMapper.map(sd, CustomShipmentSyncRequest.class);
       modelMapper.map(temp, cs);

        // assigning root level properties not previously mapped
        cs.setReferenceNo(sd.getBookingReference());
        cs.setCustom_ShipType(sd.getDirection());
        cs.setContainerType(sd.getShipmentType());
        cs.setStatusString(sd.getStatus().toString());
        cs.setSalesAgentId(sd.getSalesAgent());
        cs.setInners(sd.getInnerPacks());
        cs.setInnersUnit(sd.getInnerPackUnit());
        cs.setMarksnNums(sd.getMarksNum());
        cs.setConsolidationReferenceNumber(sd.getConsolRef());
        cs.setChargeable(sd.getChargable());
        cs.setChargableUnit(sd.getChargeableUnit());
        cs.setPacks(sd.getNoOfPacks());

        cs.setLockedByUser(sd.getLockedBy()); // lockedBy also present

        cs.setFinanceClosedByUser(sd.getFinanceClosedBy());

        // Fully auto-mapped entities
        // Events, jobs, referenceNumbers, docs, elDetails, services, notes
        // packing (except OriginName field)

        //Mapping root party objects that are not auto-mapped
        cs.setConsignerParty(mapPartyObject(sd.getConsigner()));
        cs.setConsigneeParty(mapPartyObject(sd.getConsignee()));

        // assigning child entities not automatically mapped
        // entityID also gets assigned as a part of this mapping
        mapTruckDriverDetail(cs, sd);
        cs.setRoutings(convertToList(sd.getRoutingsList(), RoutingsRequestV2.class));
        cs.setReferenceNumbers(convertToList(sd.getReferenceNumbersList(), ReferenceNumbersRequestV2.class));
        cs.setPackings_(convertToList(sd.getPackingList(), PackingRequestV2.class));
        cs.setDocs_(convertToList(sd.getFileRepoList(), FileRepoRequestV2.class));
        cs.setELDetails(convertToList(sd.getElDetailsList(), ElDetailsRequestV2.class));

        // Container missing mappings
        // dgClassString, IsHazardous, MarksnNums
        // PickupAddressJSON and DeliveryAddressJSON (could be renamed for easy mapping)

        cs.setBookingCarriages(convertToList(sd.getBookingCarriagesList(), BookingCarriageRequestV2.class));

        String finalCs = jsonHelper.convertToJson(cs);
//        retryTemplate.execute(ctx -> {
//            log.info("Current retry : {}", ctx.getRetryCount());
//            HttpEntity<V1DataResponse> entity = new HttpEntity(finalCs, V1AuthHelper.getHeaders());
//            var response = this.restTemplate.postForEntity(this.SHIPMENT_V1_SYNC_URL, entity, V1DataResponse.class, new Object[0]);
//            return response;
//        });

        try {
            HttpEntity<V1DataResponse> entity = new HttpEntity(finalCs, V1AuthHelper.getHeaders());
            var response = this.restTemplate.postForEntity(this.SHIPMENT_V1_SYNC_URL, entity, V1DataResponse.class, new Object[0]);
//            return response;
            log.info("{}", response);
        } catch (Exception e){
            log.error("{}", e.getMessage());
        }

        return ResponseHelper.buildSuccessResponse(modelMapper.map(cs, CustomShipmentSyncResponse.class));
    }
    
    public ResponseEntity<?> reverseSync(CustomShipmentSyncRequest cs) {
        String responseMsg;
        try {
            ShipmentDetails sd = modelMapper.map(cs, ShipmentDetails.class);

            mapCarrierDetailsReverse(cs, sd);
            mapAdditionalDetailsReverse(cs, sd);

//            // Clarity required
//            if(cs.getStatusString() != null && !cs.getStatusString().isEmpty()){
//                sd.setStatus(Integer.parseInt(cs.getStatusString())); // ENUM MAPPING ?
//            }

            sd.setBookingReference(cs.getReferenceNo());
            sd.setDirection(cs.getCustom_ShipType());
            sd.setShipmentType(cs.getContainerType());
            sd.setSalesAgent(cs.getSalesAgentId());
            sd.setInnerPacks(cs.getInners());
            sd.setInnerPackUnit(cs.getInnersUnit());
            sd.setMarksNum(cs.getMarksnNums());
            sd.setConsolRef(cs.getConsolidationReferenceNumber());
            sd.setChargable(cs.getChargeable());
            sd.setChargeableUnit(cs.getChargableUnit());
            sd.setNoOfPacks(cs.getPacks());
            sd.setFinanceClosedBy(cs.getFinanceClosedByUser());

            sd.setConsigner(mapPartyObject(cs.getConsignerParty()));
            sd.setConsignee(mapPartyObject(cs.getConsigneeParty()));

            mapTruckDriverDetailReverse(cs, sd);
            sd.setRoutingsList(convertToList(cs.getRoutings(), Routings.class));
            sd.setReferenceNumbersList(convertToList(cs.getReferenceNumbers(), ReferenceNumbers.class));
            sd.setPackingList(convertToList(cs.getPackings_(), Packing.class));
            sd.setFileRepoList(convertToList(cs.getDocs_(), FileRepo.class));
            sd.setElDetailsList(convertToList(cs.getELDetails(), ELDetails.class));

            sd.setBookingCarriagesList(convertToList(cs.getBookingCarriages(), BookingCarriage.class));

            return shipmentService.completeV1ShipmentCreateAndUpdate(CommonRequestModel.
                    buildRequest(modelMapper.map(sd, ShipmentRequest.class)));
        } catch (Exception e){
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private PartyRequestV2 mapPartyObject(Parties sourcePartyObject) {
        if(sourcePartyObject == null)
            return null;
        return modelMapper.map(sourcePartyObject, PartyRequestV2.class);
    }

    private Parties mapPartyObject(PartyRequestV2 sourcePartyObject) {
        if(sourcePartyObject == null)
            return null;
        return modelMapper.map(sourcePartyObject, Parties.class);
    }

    private void mapTruckDriverDetail(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(sd.getTruckDriverDetails() == null)
            return;

        List<TruckDriverDetailsRequestV2> req = sd.getTruckDriverDetails().stream()
                .map(item -> {
                    TruckDriverDetailsRequestV2 t;
                    t = modelMapper.map(item, TruckDriverDetailsRequestV2.class);
                    t.setTransporterNameOrg(item.getTransporterName());
                    //ENUM
                    t.setTransporterTypeString(item.getTransporterType().toString());
                    return t;
                })
                .collect(Collectors.toList());

        cs.setTruckDriverDetail(req);

    }

    private void mapTruckDriverDetailReverse(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(cs.getTruckDriverDetail() == null)
            return;

        List<TruckDriverDetails> req = cs.getTruckDriverDetail().stream()
                .map(item -> {
                    TruckDriverDetails t;
                    t = modelMapper.map(item, TruckDriverDetails.class);
                    t.setTransporterName(item.getTransporterNameOrg());
                    t.setTransporterType(Ownership.valueOf(item.getTransporterTypeString()));
                    return t;
                })
                .toList();
        sd.setTruckDriverDetails(req);
    }

    private void mapCarrierDetails(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(sd.getCarrierDetails() == null)
            return;
        modelMapper.typeMap(CarrierDetails.class, CustomShipmentSyncRequest.class)
                .addMappings(mp -> mp.skip(CustomShipmentSyncRequest::setDestination))
                .map(sd.getCarrierDetails(), cs);
        cs.setDestinationName(sd.getCarrierDetails().getDestination());
        cs.setDestinationPortName(sd.getCarrierDetails().getDestinationPort());
        cs.setOriginName(sd.getCarrierDetails().getOrigin());
        cs.setOriginPortName(sd.getCarrierDetails().getOriginPort());

    }

    private void mapCarrierDetailsReverse(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        CarrierDetails carrierDetails = modelMapper.map(cs, CarrierDetails.class);
        carrierDetails.setDestination(cs.getDestinationName());
        carrierDetails.setDestinationPort(cs.getDestinationPortName());
        carrierDetails.setOrigin(cs.getOriginName());
        carrierDetails.setOriginPort(cs.getOriginPortName());
        sd.setCarrierDetails(carrierDetails);
    }

    private void mapAdditionalDetails(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        if(sd.getAdditionalDetails() == null)
            return;
        modelMapper.map(sd.getAdditionalDetails(), cs);
        cs.setReceivingForwarderParty(mapPartyObject(sd.getAdditionalDetails().getReceivingForwarder()));
        cs.setSendingForwarderParty(mapPartyObject(sd.getAdditionalDetails().getSendingForwarder()));
        cs.setTraderOrSupplierParty(mapPartyObject(sd.getAdditionalDetails().getTraderOrSupplier()));
    }

    private void mapAdditionalDetailsReverse(CustomShipmentSyncRequest cs, ShipmentDetails sd) {
        AdditionalDetails additionalDetails = modelMapper.map(cs, AdditionalDetails.class);
        additionalDetails.setReceivingForwarder(mapPartyObject(cs.getReceivingForwarderParty()));
        additionalDetails.setSendingForwarder(mapPartyObject(cs.getSendingForwarderParty()));
        additionalDetails.setTraderOrSupplier(mapPartyObject(cs.getTraderOrSupplierParty()));
        sd.setAdditionalDetails(additionalDetails);
    }

    private <T,P> List<P> convertToList(final List<T> lst, Class<P> clazz) {
        if(lst == null)
            return null;
        return  lst.stream()
                .map(item -> convertToClass(item, clazz))
                .collect(Collectors.toList());
    }
    private  <T,P> P convertToClass(T obj, Class<P> clazz) {
        return modelMapper.map(obj, clazz);
    }
}
