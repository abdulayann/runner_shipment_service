package com.dpw.runner.shipment.services.service.TO.impl;

import com.dpw.runner.shipment.services.dto.TO.fzb.*;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.entity.IncludedAccountingNote;
import com.dpw.runner.shipment.services.entityTO.IntegrationEntity;
import com.dpw.runner.shipment.services.entity.SpecifiedAddressLocation;
import com.dpw.runner.shipment.services.entity.enums.MeasureUnit;
import com.dpw.runner.shipment.services.entity.enums.MessageType;
import com.dpw.runner.shipment.services.entity.enums.StatusType;
import com.dpw.runner.shipment.services.dto.TO.fwb.HandlingSPHInstructions;
import com.dpw.runner.shipment.services.dto.TO.fwb.HandlingSSRInstructions;
import com.dpw.runner.shipment.services.service.TO.AbstractMessageService;
import com.dpw.runner.shipment.services.service.TO.request.AwbData;
import com.dpw.runner.shipment.services.service.TO.request.AwbKafkaEntity;
import com.dpw.runner.shipment.services.service.TO.request.MetaData;
import com.dpw.runner.shipment.services.service.TO.request.TenantInfoMeta;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.util.Strings;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.utils.ParseUtils.getUTCTimeZone;
import static com.dpw.runner.shipment.services.utils.ParseUtils.getValueFromMap;
import static com.dpw.runner.shipment.services.utils.WeightConverter.convertToKg;


@Service
public class FZBService extends AbstractMessageService<AwbData, EFZBPayload> {


    @Override
    protected MessageType getMessageType() {
        return MessageType.FZB;
    }

    @Override
    protected IntegrationEntity createEntity(AwbData awbData, EFZBPayload efzbPayload) {
        return IntegrationEntity.builder()
                .messageType(MessageType.FZB)
                .entityId(awbData.getGuid())
                .status(StatusType.SUBMITTED)
                .referenceNumber(awbData.getAwbKafkaEntity().getShipmentId())
                .awbNumber(awbData.getAwbNumber())
                .uniqueId(efzbPayload.getMessageHeaderDocument().getId())
                .inPayload(jsonHelper.serialize(awbData))
                .outPayload(jsonHelper.serialize(efzbPayload))
                .build();
    }

    @Override
    public EFZBPayload convertPayload(AwbData data) {
        if (data.getAwbShipmentInfo() == null)
            throw new RuntimeException("Invalid Data");
        EFZBPayload fzb = new EFZBPayload();
        MessageHeaderDocumentFZB mHD = prepareMHD(data);
        BusinessHeaderDocumentFZB bHD = prepareBHD(data);
        MasterConsignmentFZB cons = prepareMasterConsignment(data);
        fzb.setMessageHeaderDocument(mHD);
        fzb.setBusinessHeaderDocument(bHD);
        fzb.setMasterConsignment(cons);
        return fzb;
    }

    private MasterConsignmentFZB prepareMasterConsignment(AwbData data) {
        MasterConsignmentFZB m = new MasterConsignmentFZB();
        AwbShipmentInfo shipInfo = data.getAwbShipmentInfo();


        LocationFZB origin = LocationFZB.builder()
                .name(shipInfo.getOriginAirport()).build();
        Optional.ofNullable(data.getMeta()).map(MetaData::getPol).ifPresent(d->origin.setId(d.getIataCode()));

        m.setOriginLocation(origin);
        LocationFZB destination = LocationFZB.builder().
               name(shipInfo.getDestinationAirport()).build();
        Optional.ofNullable(data.getMeta()).map(MetaData::getPod).ifPresent(e->destination.setId(e.getIataCode()));
        m.setFinalDestinationLocation(destination);

        IncludedHouseConsignmentFZB house = prepareHouse(data);
        m.setIncludedHouseConsignment(house);

        m.setTotalPieceQuantity(house.getTotalPieceQuantity());
        m.setIncludedTareGrossWeightMeasure(house.getIncludedTareGrossWeightMeasure());
        m.setIncludedTareGrossWeightMeasureUnit(house.getIncludedTareGrossWeightMeasureUnit());

        m.setTransportContractDocumentId(data.getMeta().getMasterAwbNumber());

        return m;
    }

    private IncludedHouseConsignmentFZB prepareHouse(AwbData data) {
        MetaData meta = data.getMeta();
        IncludedHouseConsignmentFZB h = new IncludedHouseConsignmentFZB();
        AwbShipmentInfo shipInfo = data.getAwbShipmentInfo();
        AwbKafkaEntity ent = data.getAwbKafkaEntity();
        AwbCargoInfo awbCargoInfo = data.getAwbCargoInfo();
        AwbPaymentInfo awbPaymentInfo = data.getAwbPaymentInfo();

        Optional.ofNullable(ent).ifPresent(v -> h.setId(v.getShipmentId()));
        if (Objects.nonNull(awbCargoInfo) && Objects.nonNull(awbCargoInfo.getChargeCode())) {
            Boolean indicator = false;
            if (awbCargoInfo.getChargeCode().startsWith("P")) {
                indicator = true;
            }
            h.setTotalChargePrepaidIndicator(indicator);
            h.setTotalDisbursementPrepaidIndicator(indicator);
        }

        if (Objects.nonNull(awbPaymentInfo)) {
            h.setWeightTotalChargeAmount(awbPaymentInfo.getWeightCharges().doubleValue());
            h.setValuationTotalChargeAmount(awbPaymentInfo.getValuationCharge().doubleValue());
            h.setTaxTotalChargeAmount(awbPaymentInfo.getTax().doubleValue());
            h.setAgentTotalDisbursementAmount(awbPaymentInfo.getDueAgentCharges().doubleValue());
            h.setCarrierTotalDisbursementAmount(awbPaymentInfo.getDueCarrierCharges().doubleValue());
            h.setTotalPrepaidChargeAmount(awbPaymentInfo.getTotalPrepaid().doubleValue());
            h.setTotalCollectChargeAmount(awbPaymentInfo.getTotalCollect().doubleValue());
        }
        h.setAmountCurrency(getCurrency(data));
        setGoodsMetaData(data, h);
        h.setSummaryDescription(data.getAwbCargoInfo().getNtrQtyGoods());

        PartyFZB consignor = new PartyFZB();
        consignor.setName(StringUtils.substring(shipInfo.getShipperName(), 0 ,35));
        PostalStructuredAddressFZB partyAddress = new PostalStructuredAddressFZB();
        if (Objects.nonNull(meta.getShipper())) {
            partyAddress.setCountryID(meta.getShipper().country);
            partyAddress.setCityName(StringUtils.substring(meta.getShipper().getCity(), 0 ,17));
//            partyAddress.setStreetName(StringUtils.substring(meta.getShipper().getCity(), 0 ,35));
        }
        if (Objects.nonNull(data.getAwbShipmentInfo()))
            partyAddress.setStreetName(StringUtils.substring(data.getAwbShipmentInfo().getShipperAddress(), 0 ,35));

        consignor.setPostalStructuredAddress(partyAddress);
        h.setConsignorParty(consignor);

        PartyFZB consignee = new PartyFZB();
        consignee.setName(StringUtils.substring(shipInfo.getConsigneeName(), 0 ,35));
        PostalStructuredAddressFZB partyAddressconsignee = new PostalStructuredAddressFZB();
        if (Objects.nonNull(meta.getConsignee())) {
            partyAddressconsignee.setCountryID(meta.getConsignee().country);
            partyAddressconsignee.setCityName(StringUtils.substring(meta.getConsignee().getCity(), 0 ,17));
//            partyAddressconsignee.setStreetName(StringUtils.substring(meta.getConsignee().getCity(), 0 ,35));
        }
        if (Objects.nonNull(data.getAwbShipmentInfo()))
            partyAddressconsignee.setStreetName(StringUtils.substring(data.getAwbShipmentInfo().getConsigneeAddress(), 0 ,35));

        consignee.setPostalStructuredAddress(partyAddressconsignee);
        h.setConsigneeParty(consignee);

        LocationFZB origin = LocationFZB.builder()
                .name(shipInfo.getOriginAirport()).build();
        Optional.ofNullable(data.getMeta()).map(MetaData::getPol).ifPresent(e->origin.setId(e.getIataCode()));
        h.setOriginLocation(origin);

        LocationFZB destination = LocationFZB.builder().
                name(shipInfo.getDestinationAirport()).build();
        Optional.ofNullable(data.getMeta()).map(MetaData::getPod).ifPresent(e->destination.setId(e.getIataCode()));
        h.setFinalDestinationLocation(destination);

        if (Objects.nonNull(data.getMeta()))
            h.setSpecifiedLogisticsTransportMovement(prepareMultiRout(data.getMeta().getAwbRoutingInfo()));

        h.setHandlingSPHInstructions(prepareSpecialCode(data.getAwbSpecialHandlingCodesMappings()));


        if (Objects.nonNull(awbCargoInfo) && Objects.nonNull(awbCargoInfo.getHandlingInfo())) {
            List<HandlingSSRInstructions> ssr = new ArrayList<>();
            ssr.add(HandlingSSRInstructions.builder()
                    .description(awbCargoInfo.getHandlingInfo()).build());
            h.setHandlingSSRInstructions(ssr);
        }

        if (Objects.nonNull(awbCargoInfo) && Objects.nonNull(awbCargoInfo.getAccountingInfo())) {
            List<IncludedAccountingNote> acc = new ArrayList<>();
            acc.add(IncludedAccountingNote.builder()
                    .contentCode("GEN").content(awbCargoInfo.getAccountingInfo()).build());
            h.setIncludedAccountingNotes(acc);
        }

        if (Objects.nonNull(data.getMeta().getIssueingAgent())) {
            h.setIncludedCustomsNote(List.of(CustomsNoteFZB.builder().content(data.getMeta().getIssueingAgent().getNumber()).build()));
        }

        h.setIncludedHouseConsignmentItem(prepareHouseConsignment(data));
        return h;

    }

    private List<HouseConsignmentItemFZB> prepareHouseConsignment(AwbData data) {
        AwbCargoInfo cargoInfo = data.getAwbCargoInfo();
        if (Objects.isNull(data.getAwbGoodsDescriptionInfo()) && data.getAwbGoodsDescriptionInfo().size() < 1 )
            return null;
        List<HouseConsignmentItemFZB> goods = new ArrayList<>();
        Integer seq = 1;
        for(AwbGoodsDescriptionInfo good : data.getAwbGoodsDescriptionInfo()) {
            HouseConsignmentItemFZB g = new HouseConsignmentItemFZB();
            g.setSequenceNumeric(seq++);

            if (Objects.nonNull(good.getGrossWt()) && Strings.isNotBlank(good.getGrossWtUnit())) {
                g.setGrossWeightMeasure(WeightMeasureFZB.builder()
                        .value(convertToKg(good.getGrossWt().doubleValue(), good.getGrossWtUnit()))
                        .unitCode(MeasureUnit.KGM.name()).build());
            }
            g.setPieceQuantity(good.getPiecesNo());

            if (Objects.nonNull(good.getAwbPackingInfo())) {

                Boolean haz = good.getAwbPackingInfo().get(0).getHazardous();
                if (Objects.nonNull(haz) && haz == Boolean.TRUE)
                    g.setNatureIdentificationTransportCargo("Hazardous Goods");
                else
                    g.setNatureIdentificationTransportCargo("Normal Goods");
            }

            Optional.ofNullable(cargoInfo).ifPresent(c -> g.setOriginCountryId(data.getAwbCargoInfo().getCustomOriginCode()));

            FreightRateServiceChargeFZB appFr = new FreightRateServiceChargeFZB();
            appFr.setCategoryCode(getValueFromMap(String.valueOf(good.getRateClass()), data.getMeta().getRateClass()));
            if (Objects.nonNull(good.getChargeableWt()) && Strings.isNotBlank(good.getGrossWtUnit())) {
                appFr.setChargeableWeightMeasure(convertToKg(good.getChargeableWt().doubleValue(), good.getGrossWtUnit()));
                appFr.setChargeableWeightMeasureUnit(MeasureUnit.KGM.name());
            }
            appFr.setAppliedRate(good.getRateCharge().doubleValue());
            appFr.setAppliedAmount(good.getTotalAmount().doubleValue());
            appFr.setAppliedAmountCurrency(getCurrency(data));

            g.setApplicableFreightRateServiceCharge(appFr);
            goods.add(g);
        }
            return goods;
    }

    private List<HandlingSPHInstructions> prepareSpecialCode(ArrayList<AwbSpecialHandlingCodesMappingInfo> awbSpecialHandlingCodesMappings) {
        return Optional.ofNullable(awbSpecialHandlingCodesMappings)
                .filter(list -> !list.isEmpty())
                .map(list -> list.stream()
                        .map(s -> HandlingSPHInstructions.builder()
                                .descriptionCode(s.getShcId())
                                .build())
                        .collect(Collectors.toList()))
                .orElse(null);
    }

    private List<LogisticsTransportMovementFZB> prepareMultiRout(List<AwbRoutingInfo> awbRoutingInfo) {
        if (Objects.isNull(awbRoutingInfo) || awbRoutingInfo.size() < 1)
            return null;
        List<LogisticsTransportMovementFZB> routingList = new ArrayList<>();
        Integer seq = 1;
        for (AwbRoutingInfo rout : awbRoutingInfo) {
            LogisticsTransportMovementFZB r = new LogisticsTransportMovementFZB();
            r.setStageCode("Main-Carriage");
            r.setMode("4");
            r.setMode("AIR TRANSPORT");
            if (Objects.nonNull(rout.getAirlineInfo()))
                r.setID(rout.getAirlineInfo().getIata() + rout.getFlightNumber());
            r.setSequenceNumeric(seq++);


            EventFZB dep = new EventFZB();
            dep.setScheduledOccurrenceDateTime(rout.getFlightDate());
            SpecifiedAddressLocation depLoc = SpecifiedAddressLocation.builder().
                    name(rout.getOriginPortUnlocName()).build();
            dep.setSpecifiedAddressLocation(depLoc);
            r.setDepartureEvent(dep);

            //ToDO-not required
            EventFZB arr = new EventFZB();
            SpecifiedAddressLocation desLoc = SpecifiedAddressLocation.builder()
                    .name(rout.getDestinationPortUnlocName()).build();
            arr.setSpecifiedAddressLocation(desLoc);
            r.setArrivalEvent(arr);

            routingList.add(r);

        }

        return routingList;
    }

    private void setGoodsMetaData(AwbData data, IncludedHouseConsignmentFZB h) {
        AwbCargoInfo awbCargoInfo = data.getAwbCargoInfo();
        ArrayList<AwbGoodsDescriptionInfo> goodsList = data.getAwbGoodsDescriptionInfo();
        if (Objects.nonNull(goodsList)) {
            Double grossWeight = 0.0;
            Integer totalPieces = 0;
            Double totalCharge = 0.0;
            for (AwbGoodsDescriptionInfo goods : goodsList) {
                if (Objects.nonNull(goods.getGrossWt()) && StringUtils.isNotBlank(goods.getGrossWtUnit()))
                    grossWeight += convertToKg(goods.getChargeableWt().doubleValue(), goods.getGrossWtUnit());
                if (Objects.nonNull(goods.getPiecesNo()))
                    totalPieces += goods.getPiecesNo();
                if (Objects.nonNull(goods.getTotalAmount()))
                    totalCharge += goods.getTotalAmount().doubleValue();
            }
            if (grossWeight > 0) {
                h.setIncludedTareGrossWeightMeasure(grossWeight.toString());
                h.setIncludedTareGrossWeightMeasureUnit(MeasureUnit.KGM.name());
            }
            if (totalPieces > 0 ) {
                h.setTotalPieceQuantity(totalPieces);
            }
        }
    }

    private BusinessHeaderDocumentFZB prepareBHD(AwbData data) {
        BusinessHeaderDocumentFZB bHD = new BusinessHeaderDocumentFZB();
        AwbShipmentInfo shipInfo = data.getAwbShipmentInfo();
        bHD.setId(shipInfo.getAwbNumber());

        Optional.ofNullable(data.getMeta()).map(MetaData::getUserInfo).ifPresent(v-> {
            bHD.setSignatoryConsignorAuthenticationName(StringUtils.substring(v.getUserName(), 0 ,20));
        });
        SignatoryCarrierAuthenticationFZB ss = new SignatoryCarrierAuthenticationFZB();
        ss.setActualDateTime(getUTCTimeZone());
        Optional.ofNullable(data.getMeta()).map(MetaData::getTenantInfo).map(TenantInfoMeta::getBranchName).ifPresent(v->{
            if (v.length() > 20)
                ss.setSignatory(v.substring(0, 20));
            else
                ss.setSignatory(v);
        });
        Optional.ofNullable(data.getMeta()).map(MetaData::getTenantInfo).ifPresent(v->ss.setIssueAuthenticationLocationName(v.getCity()));
        bHD.setSignatoryCarrierAuthentication(ss);

        return bHD;
    }

    private MessageHeaderDocumentFZB prepareMHD(AwbData data) {
        MessageHeaderDocumentFZB mHD = new MessageHeaderDocumentFZB();
        AwbShipmentInfo shipInfo = data.getAwbShipmentInfo();
        mHD.setId(StringUtils.substring(shipInfo.getAwbNumber() + "_" + LocalDateTime.now(), 0, 70));
        mHD.setName("House waybill");
        mHD.setTypeCode("703");
        mHD.setPurposeCode("Creation");
        mHD.setVersionID("3.00");

        mHD.setIssueDateTime(getUTCTimeZone());

        List<PartyDtoFZB> senderParty = new ArrayList<>();
        senderParty.add(PartyDtoFZB.builder().schemeID("C").value(data.getMeta().getTenantInfo().getPimaAddress()).build());
        mHD.setSenderParty(senderParty);

        List<PartyDtoFZB>  receipientParty = new ArrayList<>();
        receipientParty.add(PartyDtoFZB.builder().schemeID("C").value("TDVSYS03GLNUNADDR").build());
        mHD.setReceiverParty(receipientParty);

        return mHD;
    }

    private String getCurrency(AwbData data) {
        return data.getAwbCargoInfo().getCurrency();
    }

}
