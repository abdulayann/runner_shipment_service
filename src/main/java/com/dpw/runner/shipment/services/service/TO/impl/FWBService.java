package com.dpw.runner.shipment.services.service.TO.impl;

import com.dpw.runner.shipment.services.commons.EAWBConstants;
import com.dpw.runner.shipment.services.dto.TO.fwb.*;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.entity.IncludedAccountingNote;
import com.dpw.runner.shipment.services.entityTO.IntegrationEntity;
import com.dpw.runner.shipment.services.entity.SpecifiedAddressLocation;
import com.dpw.runner.shipment.services.entity.enums.MeasureUnit;
import com.dpw.runner.shipment.services.entity.enums.MessageType;
import com.dpw.runner.shipment.services.entity.enums.RatingTypeIndicator;
import com.dpw.runner.shipment.services.entity.enums.StatusType;
import com.dpw.runner.shipment.services.service.TO.AbstractMessageService;
import com.dpw.runner.shipment.services.service.TO.request.AwbData;
import com.dpw.runner.shipment.services.service.TO.request.AwbKafkaEntity;
import com.dpw.runner.shipment.services.service.TO.request.MetaData;
import com.dpw.runner.shipment.services.service.TO.request.TenantInfoMeta;
import com.dpw.runner.shipment.services.utils.StringUtility;
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
import static com.dpw.runner.shipment.services.utils.WeightConverter.round;


@Service
public class FWBService extends AbstractMessageService<AwbData, EAWBPayload> {


    @Override
    protected MessageType getMessageType() {
        return MessageType.FWB;
    }

    @Override
    protected IntegrationEntity createEntity(AwbData awbData, EAWBPayload eawbPayload) {
        return IntegrationEntity.builder()
                .messageType(MessageType.FWB)
                .entityId(awbData.getGuid())
                .status(StatusType.SUBMITTED)
                .referenceNumber(awbData.getAwbKafkaEntity().getShipmentId())
                .awbNumber(awbData.getAwbNumber())
                .uniqueId(eawbPayload.getMessageHeaderDocument().getId())
                .inPayload(jsonHelper.serialize(awbData))
                .outPayload(jsonHelper.serialize(eawbPayload))
        .build();
    }

    @Override
    public EAWBPayload convertPayload(AwbData data) {
        if (data.getAwbShipmentInfo() == null)
            throw new RuntimeException("Invalid Data");

        EAWBPayload p = new EAWBPayload();
        MessageHeaderDocument mHD = prepareMHD(data);
        BusinessHeaderDocument bHD = prepareBHD(data);
        MasterConsignment masterConsignment = prepareMasterCons(data);
        p.setMessageHeaderDocument(mHD);
        p.setBusinessHeaderDocument(bHD);
        p.setMasterConsignment(masterConsignment);
        return p;
    }

    private MasterConsignment prepareMasterCons(AwbData data) {
        MetaData meta = data.getMeta();
        MasterConsignment m = new MasterConsignment();
        AwbShipmentInfo shipInfo = data.getAwbShipmentInfo();
        AwbKafkaEntity awbKafkaEntity = data.getAwbKafkaEntity();
        AwbCargoInfo awbCargoInfo = data.getAwbCargoInfo();
        Optional.ofNullable(awbKafkaEntity).ifPresent(a -> m.setId(a.getShipmentId()));
        if (Objects.nonNull(awbCargoInfo) && Objects.nonNull(awbCargoInfo.getChargeCode())) {
            Boolean indicator = false;
            if (awbCargoInfo.getChargeCode().startsWith("P")) {
                indicator = true;
            }
            m.setTotalChargePrepaidIndicator(indicator);
            m.setTotalDisbursementPrepaidIndicator(indicator);
        }
        fetchGoodsMetaData(data, m);

        Party consignor = new Party();
        consignor.setName(StringUtils.substring(shipInfo.getShipperName(), 0 ,70));
        PartyAddress partyAddress = new PartyAddress();
        if (meta != null && Objects.nonNull(meta.getShipper())) {
            partyAddress.setCountryID(meta.getShipper().country);
            partyAddress.setCityName(StringUtils.substring(meta.getShipper().getCity(), 0 ,17));
            if (Objects.nonNull(data.getAwbShipmentInfo()))
                partyAddress.setStreetName(StringUtils.substring(data.getAwbShipmentInfo().getShipperAddress(), 0 ,70));
        }

        consignor.setPartyAddress(partyAddress);
        m.setConsignorParty(consignor);

        Party consignee = new Party();
        consignee.setName(StringUtils.substring(shipInfo.getConsigneeName(), 0 ,70));
        PartyAddress consigneeAdd = new PartyAddress ();
        if (meta != null && Objects.nonNull(meta.getConsignee())) {
            consigneeAdd.setCountryID(meta.getConsignee().country);
            consigneeAdd.setCityName(StringUtils.substring(meta.getConsignee().getCity(), 0 ,17));
            if (Objects.nonNull(data.getAwbShipmentInfo()))
                consigneeAdd.setStreetName(StringUtils.substring(data.getAwbShipmentInfo().getConsigneeAddress(), 0 ,70));
        }
        consignee.setPartyAddress(consigneeAdd);
        m.setConsigneeParty(consignee);

        LocationDto origin = LocationDto.builder()
                .name(shipInfo.getOriginAirport()).build();
        Optional.ofNullable(data.getMeta()).map(MetaData::getPol).ifPresent(v-> origin.setId(v.getIataCode()));
        m.setOriginLocation(origin);

        LocationDto destination = LocationDto.builder()
                .name(shipInfo.getDestinationAirport()).build();
        Optional.ofNullable(data.getMeta()).map(MetaData::getPod).ifPresent(e->destination.setId(e.getIataCode()));
        m.setFinalDestinationLocation(destination);

        if (Objects.nonNull(data.getMeta()))
            m.setSpecifiedLogisticsTransportMovement(prepareMultiRouting(data.getMeta().getAwbRoutingInfo()));
        m.setHandlingSPHInstructions(prepareSpecialCode(data.getAwbSpecialHandlingCodesMappings()));

        if (Objects.nonNull(awbCargoInfo) && Objects.nonNull(awbCargoInfo.getHandlingInfo())) {
            List<HandlingSSRInstructions> ssr = new ArrayList<>();
            ssr.add(HandlingSSRInstructions.builder()
                    .description(awbCargoInfo.getHandlingInfo()).build());
            m.setHandlingSSRInstructions(ssr);
        }

        if (Objects.nonNull(awbCargoInfo) && Objects.nonNull(awbCargoInfo.getAccountingInfo())) {
            List<IncludedAccountingNote> acc = new ArrayList<>();
            acc.add(IncludedAccountingNote.builder()
                    .contentCode("GEN").content(awbCargoInfo.getAccountingInfo()).build());
            m.setIncludedAccountingNotes(acc);
        }

        if (Objects.nonNull(data.getMeta()) && Objects.nonNull(data.getMeta().getIssueingAgent())) {
            m.setIncludedCustomsNote(List.of(IncludedCustomsNote.builder().content(data.getMeta().getIssueingAgent().getNumber()).build()));
        }


        m.setApplicableOriginCurrencyExchangeCode(getCurrency(data));
        //ToDO - Optional but still check applicableDestinationCurrencyExchange

        m.setApplicableLogisticsAllowanceCharges(prepareOTH(data));

        m.setApplicableRatings(prepareApplicableRating(data));
        
        m.setApplicableTotalRating(prepareApplicableTotalRating(data));

        return m;
    }

    private List<ApplicableTotalRating> prepareApplicableTotalRating(AwbData data) {
        if (Objects.isNull(data.getAwbPaymentInfo()))
            return null;
        AwbPaymentInfo pay = data.getAwbPaymentInfo();
        List<ApplicableTotalRating> tr = new ArrayList<>();
        ApplicableTotalRating agg = new ApplicableTotalRating();
        agg.setTypeCode(RatingTypeIndicator.P);
        List<ApplicablePrepaidCollectMonetarySummation> totalAmount = new ArrayList<>();
        ApplicablePrepaidCollectMonetarySummation am = new ApplicablePrepaidCollectMonetarySummation();

        Boolean indicator = false;
        if (Objects.nonNull(data.getAwbCargoInfo().getChargeCode()) && data.getAwbCargoInfo().getChargeCode().startsWith("P"))
            indicator = true;
        am.setPrepaidIndicator(indicator);

        if (Objects.nonNull(pay.getWeightCharges())) {
            am.setWeightChargeTotalAmount(pay.getWeightCharges().doubleValue());
            am.setWeightChargeTotalAmountCurrency(getCurrency(data));
        }

        if (Objects.nonNull(pay.getValuationCharge())) {
            am.setValuationChargeTotalAmount(pay.getValuationCharge().doubleValue());
            am.setValuationChargeTotalAmountCurrency(getCurrency(data));
        }

        if (Objects.nonNull(pay.getTax())) {
            am.setTaxTotalAmount(pay.getTax().doubleValue());
            am.setTaxTotalAmountCurrency(getCurrency(data));
        }

        if (Objects.nonNull(pay.getDueAgentCharges())) {
            am.setAgentTotalDuePayableAmount(pay.getDueAgentCharges().doubleValue());
            am.setAgentTotalDuePayableAmountCurrency(getCurrency(data));
        }

        if (Objects.nonNull(pay.getDueCarrierCharges())) {
            am.setCarrierTotalDuePayableAmount(pay.getDueCarrierCharges().doubleValue());
            am.setCarrierTotalDuePayableAmountCurrency(getCurrency(data));
        }

        if (Objects.nonNull(data.getMeta())) {
            am.setGrandTotalAmount(data.getMeta().getTotalAmount());
            am.setGrandTotalAmountCurrency(getCurrency(data));
        }

        totalAmount.add(am);
        agg.setApplicablePrepaidCollectMonetarySummation(totalAmount);
        tr.add(agg);
        return tr;
    }

    private List<ApplicableRating> prepareApplicableRating(AwbData data) {

        if (Objects.isNull(data.getAwbGoodsDescriptionInfo()) && data.getAwbGoodsDescriptionInfo().size() < 1 )
            return null;

        List<ApplicableRating> r = new ArrayList<>();
        ApplicableRating rating = new ApplicableRating();
        rating.setTypeCode(RatingTypeIndicator.P);
        List<IncludedMasterConsignmentItem> goods = new ArrayList<>();
        Integer seq = 1;
        Double goodsTotalAmount = 0.0;
        for(AwbGoodsDescriptionInfo good : data.getAwbGoodsDescriptionInfo()) {
            IncludedMasterConsignmentItem i = new IncludedMasterConsignmentItem();
            i.setSequenceNumeric(seq++);
            i.setTypeCode(good.getHsCode() != null ? good.getHsCode() : null);
            if (Objects.nonNull(good.getGrossWt()) && Strings.isNotBlank(good.getGrossWtUnit())) {
                i.setGrossWeightMeasure(convertToKg(good.getGrossWt().doubleValue(), good.getGrossWtUnit()));
                i.setGrossWeightMeasureUnit(MeasureUnit.KGM.name());
            }
            i.setPieceQuantity(good.getPiecesNo());
            i.setInformation("NDA");
            //ToDo optionall field
//            if (Objects.nonNull(good.getAwbPackingInfo()) && good.getAwbPackingInfo().size() > 1) {
//                List<TransportLogisticsPackage> packages = new ArrayList<>();
//                for(AwbPackingInfo p : good.getAwbPackingInfo()) {
//                    TransportLogisticsPackage pack = new TransportLogisticsPackage();
//                    pack.setItemQuantity(p.getPacks());
//                    packages.add(pack);
//                }
//
//                i.setTransportLogisticsPackageList(packages);
//            }
            ApplicableFreightRateServiceCharge appFr = new ApplicableFreightRateServiceCharge();
            appFr.setCategoryCode(getValueFromMap(String.valueOf(good.getRateClass() != null ? good.getRateClass() : null), data.getMeta() != null ? data.getMeta().getRateClass() : null));
            if (Objects.nonNull(good.getChargeableWt()) && Strings.isNotBlank(good.getGrossWtUnit())) {
                appFr.setChargeableWeightMeasure(convertToKg(good.getChargeableWt().doubleValue(), good.getGrossWtUnit()));
                appFr.setChargeableWeightMeasureUnit(MeasureUnit.KGM.name());
            }
            appFr.setAppliedRate(good.getRateCharge() != null ? good.getRateCharge().doubleValue() : null);
            appFr.setAppliedAmount(good.getTotalAmount() != null ? good.getTotalAmount().doubleValue() : null);
            appFr.setAppliedAmountCurrency(getCurrency(data));
            i.setApplicableFreightRateServiceCharge(appFr);
            goods.add(i);
            if (Objects.nonNull(good.getTotalAmount()))
                goodsTotalAmount += good.getTotalAmount().doubleValue();
        }
        if (goodsTotalAmount > 0) {
            rating.setTotalChargeAmount(goodsTotalAmount);
            rating.setTotalChargeCurrency(getCurrency(data));
        }
        rating.setIncludedMasterConsignmentItems(goods);
        r.add(rating);
        return r;
    }

    private List<ApplicableLogisticsAllowanceCharge> prepareOTH(AwbData data) {
        ArrayList<AwbOtherChargesInfo> awbOtherChargesInfo = data.getAwbOtherChargesInfo();
        //AwbShipmentInfo shipInfo = data.getAwbShipmentInfo();
        if (Objects.isNull(awbOtherChargesInfo) || awbOtherChargesInfo.size() < 1)
            return null;
        List<ApplicableLogisticsAllowanceCharge> chargeList = new ArrayList<>();
        for (AwbOtherChargesInfo oth : awbOtherChargesInfo) {
            ApplicableLogisticsAllowanceCharge ch = new ApplicableLogisticsAllowanceCharge();
            ch.setId(oth.getIataDescription());
            ch.setPrepaidIndicator(Objects.nonNull(oth.getModeOfPayment()) && oth.getModeOfPayment().startsWith("P"));


            String chargeDueValue = getValueFromMap(String.valueOf(oth.getChargeDue()), data.getMeta() != null ? data.getMeta().getChargeDue() : null);
            if (chargeDueValue != null) {
                ch.setPartyTypeCode(chargeDueValue.startsWith("A") ? "A" : "C");
            }

            ch.setActualAmount(oth.getAmount().doubleValue());
            ch.setActualAmountCurrency(getCurrency(data));


            chargeList.add(ch);

        }
        return chargeList;
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


    private List<LogisticsTransportMovement> prepareMultiRouting(ArrayList<AwbRoutingInfo> awbRoutingInfo) {
        if (Objects.isNull(awbRoutingInfo) || awbRoutingInfo.size() < 1)
            return null;

        List<LogisticsTransportMovement> r = new ArrayList<>();
        Integer seq = 1;
        for (AwbRoutingInfo rout : awbRoutingInfo) {
            LogisticsTransportMovement routing = new LogisticsTransportMovement();
            routing.setStageCode("Main-Carriage");
            routing.setModeCode("4");
            routing.setMode("AIR TRANSPORT");
            if (Objects.nonNull(rout.getAirlineInfo()))
                routing.setId(rout.airlineInfo.getIata() + rout.getFlightNumber());

            routing.setSequenceNumeric(seq++);

            DepartureEventDto dep = new DepartureEventDto();
            dep.setScheduledOccurrenceDateTime(rout.getFlightDate());
            SpecifiedAddressLocation depLoc = SpecifiedAddressLocation.builder().
                    name(rout.getOriginPortUnlocName()).build();
            dep.setSpecifiedAddressLocation(depLoc);
            routing.setDepartureEvent(dep);

            ArrivalEventDto arr = ArrivalEventDto.builder().build();
//            arr.setScheduledOccurrenceDateTime(null);
            SpecifiedAddressLocation desLoc = SpecifiedAddressLocation.builder()
                    .name(rout.getDestinationPortUnlocName()).build();
            arr.setSpecifiedAddressLocation(desLoc);
            routing.setArrivalEvent(arr);

            r.add(routing);
        }
        return r;
    }

    private void fetchGoodsMetaData(AwbData data, MasterConsignment m) {
        ArrayList<AwbGoodsDescriptionInfo> goodsList = data.getAwbGoodsDescriptionInfo();
        if (Objects.nonNull(goodsList)) {
            Double grossWeight = 0.0;
            Integer totalPieces = 0;
            for (AwbGoodsDescriptionInfo goods : goodsList) {
                if (Objects.nonNull(goods.getChargeableWt()) && StringUtils.isNotBlank(goods.getGrossWtUnit()))
                    grossWeight += convertToKg(goods.getChargeableWt().doubleValue(), goods.getGrossWtUnit());
                if (Objects.nonNull(goods.getPiecesNo()))
                    totalPieces += goods.getPiecesNo();
            }
            if (grossWeight >= 0) {
                Double weight = round(grossWeight, 3);
                m.setIncludedTareGrossWeightMeasureWeight(weight);
                m.setIncludedTareGrossWeightMeasureUnitCode("KGM");
            }
            m.setTotalPieceQuantity(totalPieces);
        }
    }

    private BusinessHeaderDocument prepareBHD(AwbData data) {
        BusinessHeaderDocument bHD = new BusinessHeaderDocument();
        AwbShipmentInfo shipInfo = data.getAwbShipmentInfo();
        bHD.setId(shipInfo.getAwbNumber());

        SignatoryCarrierAuthentication ss = new SignatoryCarrierAuthentication();
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

    private MessageHeaderDocument prepareMHD(AwbData data) {
        MessageHeaderDocument mHD = new MessageHeaderDocument();
        AwbShipmentInfo shipInfo = data.getAwbShipmentInfo();
        mHD.setId(StringUtils.substring(shipInfo.getAwbNumber() + "_" + LocalDateTime.now(), 0, 70));
        if (isDirect(shipInfo.getEntityType())) {
            mHD.setName("Air Waybill");
            mHD.setTypeCode("740");
        }
        else {
            mHD.setName("Master Air Waybill");
            mHD.setTypeCode("741");
        }

        mHD.setPurposeCode("Creation");
        mHD.setVersionID("3.00");

        mHD.setIssueDateTime(getUTCTimeZone());

        List<IDdto> senderParty = new ArrayList<>();
        senderParty.add(IDdto.builder().schemeID("C").value(data.getMeta() != null ? data.getMeta().getTenantInfo().getPimaAddress() : StringUtility.getEmptyString()).build());
        mHD.setSenderPartyIds(senderParty);

        List<IDdto> receipientParty = new ArrayList<>();
        receipientParty.add(IDdto.builder().schemeID("C").value("TDVSYS03GLNUNADDR").build());
        mHD.setRecipientPartyIds(receipientParty);
        return mHD;
    }

    private String getCurrency(AwbData data) {
        return data.getAwbCargoInfo().getCurrency();
    }

    private Boolean isDirect(String type) {
        if (Objects.equals(type, EAWBConstants.DMAWB))
            return true;
        else
            return false;
    }


}

