package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShippingInstructionModel;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IHblRepository;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.type.TypeReference;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.*;
import static java.lang.Long.sum;

@Component
public class ShippingInstructionReport extends IReport{

    @Autowired
    JsonHelper jsonHelper;
    @Autowired
    private IHblRepository hblRepository;
    @Override
    public Map<String, Object> getData(Long id) {
        ShippingInstructionModel shippingInstructionModel = (ShippingInstructionModel) getDocumentModel(id);
        return populateDictionary(shippingInstructionModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {
        return ShippingInstructionModel.builder()
                .tenant(getTenant())
                .shipment(getShipment(id))
                .containersList(getShipment(id).getContainersList())
                .build();
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ShippingInstructionModel model = (ShippingInstructionModel) documentModel;
        validateAirAndOceanDGCheck(model.getShipment());
        Map<String, Object> dictionary = new HashMap<>();

        addConsignerTags(model, dictionary);

        addConsigneeTags(model, dictionary);

        addNotifyPartyTags(model, dictionary);

        dictionary.put(MBL_NUMBER, model.getShipment().getMasterBill());
        dictionary.put(HBL_NUMBER, model.getShipment().getHouseBill());
        addCarrierDetailTags(model, dictionary);
        dictionary.put(SERVICE_TYPE, model.getShipment().getTransportMode());
        dictionary.put(PPCC, model.getShipment().getPaymentTerms());
        dictionary.put(CURRENT_DATE, convertToDPWDateFormat(LocalDateTime.now()));

        dictionary.put(SHIPPED_ONBOARD_TEXT, model.getShipment().getAdditionalDetails().getShippedOnboardText().toUpperCase());
        dictionary.put(SHIPPED_ONBOARD_DATE_DDMMMYYYY, convertToDPWDateFormat(
                model.getShipment().getAdditionalDetails().getShippedOnboardDate(), "ddMMMyyyy".toUpperCase(), false));

        dictionary.put(JOB_NUMBER, model.getShipment().getShipmentId());
        dictionary.put(TRANSPORT_MODE, model.getShipment().getTransportMode());
        dictionary.put(SHIPMENT_TYPE, model.getShipment().getShipmentType());

        processPackingListTags(model, dictionary);

        var tenant = model.getTenant();
        List<String> tenantDetails = ReportHelper.getOrgAddress(tenant.tenantName, tenant.address1, tenant.address2,
                tenant.country, tenant.zipPostCode, tenant.state);
        dictionary.put(AGENT, tenantDetails);
        ReportHelper.addTenantDetails(dictionary, model.getTenant());
        dictionary.put(TENANT, ReportHelper.getListOfStrings(tenant.tenantName, tenant.address1, tenant.address2,
                        tenant.city, tenant.state, tenant.zipPostCode, tenant.country,tenant.email, tenant.websiteUrl, tenant.phone));


        //Add Bl-details
        addBlDetails(dictionary, model.getShipment().getId());

        return dictionary;
    }

    private void processPackingListTags(ShippingInstructionModel model, Map<String, Object> dictionary) {
        long totalPacks = 0L;
        BigDecimal totalVolume = BigDecimal.ZERO;
        String unitOfTotalVolume = null;
        boolean breakFlagForVolume = false;
        BigDecimal totalWeight = BigDecimal.ZERO;
        String unitOfTotalWeight = null;
        boolean breakFlagForWeight = false;
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();

        if (model.getShipment().getPackingList() != null) {

            List<Map<String, Object>> values = new ArrayList<>();
            for (PackingModel packingModel: model.getShipment().getPackingList()) {
                values.add(jsonHelper.convertValue(packingModel, new TypeReference<>() {}));
            }

            for (var v : values) {
                totalPacks = sum(totalPacks, Long.parseLong(v.get(PACKS).toString()));

                VolumeDetails volumeDetails = updateVolumeDetails(v, totalVolume, unitOfTotalVolume, breakFlagForVolume);
                totalVolume = volumeDetails.totalVolume;
                unitOfTotalVolume = volumeDetails.unitOfTotalVolume;
                breakFlagForVolume = volumeDetails.breakFlagForVolume;

                WeightDetails weightDetails = updateWeightDetails(v, totalWeight, unitOfTotalWeight, breakFlagForWeight);
                totalWeight = weightDetails.totalWeight;
                unitOfTotalWeight = weightDetails.unitOfTotalWeight;
                breakFlagForWeight = weightDetails.breakFlagForWeight;

                formatPackingVolumes(v, v1TenantSettingsResponse);
            }

            dictionary.put(ITEMS ,values);
        }

        addWeightVolumeTagsinPacking(dictionary, totalPacks, breakFlagForVolume, totalVolume, unitOfTotalVolume, breakFlagForWeight, totalWeight, unitOfTotalWeight);
    }

    private VolumeDetails updateVolumeDetails(Map<String, Object> v, BigDecimal totalVolume, String unitOfTotalVolume, boolean breakFlagForVolume) {
        if (!breakFlagForVolume && v.get(VOLUME) != null && v.get(VOLUME_UNIT) != null) {
            if (unitOfTotalVolume == null) {
                unitOfTotalVolume = v.get(VOLUME_UNIT).toString();
                totalVolume = totalVolume.add(BigDecimal.valueOf(Double.parseDouble(StringUtility.convertToString(v.get(VOLUME)))));
            } else if (!unitOfTotalVolume.equals(v.get(VOLUME_UNIT).toString())) {
                totalVolume = BigDecimal.ZERO;
                breakFlagForVolume = true;
            } else {
                totalVolume = totalVolume.add(BigDecimal.valueOf(Double.parseDouble(StringUtility.convertToString(v.get(VOLUME)))));
            }
        }
        return new VolumeDetails(totalVolume, unitOfTotalVolume, breakFlagForVolume);
    }

    private WeightDetails updateWeightDetails(Map<String, Object> v, BigDecimal totalWeight, String unitOfTotalWeight, boolean breakFlagForWeight) {
        if (!breakFlagForWeight && v.get(WEIGHT) != null && v.get(WEIGHT_UNIT) != null) {
            if (unitOfTotalWeight == null) {
                unitOfTotalWeight = v.get(WEIGHT_UNIT).toString();
                totalWeight = totalWeight.add(BigDecimal.valueOf(Double.valueOf(v.get(WEIGHT).toString())));
            } else if (!unitOfTotalWeight.equals(v.get(WEIGHT_UNIT).toString())) {
                totalWeight = BigDecimal.ZERO;
                breakFlagForWeight = true;
            } else {
                totalWeight = totalWeight.add(BigDecimal.valueOf(Double.valueOf(v.get(WEIGHT).toString())));
            }
        }
        return new WeightDetails(totalWeight, unitOfTotalWeight, breakFlagForWeight);
    }

    private void addWeightVolumeTagsinPacking(Map<String, Object> dictionary, long totalPacks, boolean breakFlagForVolume, BigDecimal totalVolume, String unitOfTotalVolume, boolean breakFlagForWeight, BigDecimal totalWeight, String unitOfTotalWeight) {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        if(totalPacks != 0)
            dictionary.put(TOTAL_PACKS, getDPWWeightVolumeFormat(new BigDecimal(totalPacks), 0, v1TenantSettingsResponse));
        else
            dictionary.put(TOTAL_PACKS, null);

        if (breakFlagForVolume || totalVolume.equals(BigDecimal.ZERO)) {
            dictionary.put(TOTAL_PACKS_VOLUME, null);
            dictionary.put(UOTV, null);
        } else {
            dictionary.put(TOTAL_PACKS_VOLUME, convertToVolumeNumberFormat(totalVolume, v1TenantSettingsResponse));
            dictionary.put(UOTV, unitOfTotalVolume);
        }

        if (breakFlagForWeight || totalWeight.equals(BigDecimal.ZERO)) {
            dictionary.put(TOTAL_PACKS_WEIGHT, null);
            dictionary.put(UOTW, null);
        } else {
            dictionary.put(TOTAL_PACKS_WEIGHT, convertToWeightNumberFormat(totalWeight, v1TenantSettingsResponse));
            dictionary.put(UOTW, unitOfTotalWeight);
        }
    }

    private void formatPackingVolumes(Map<String, Object> v, V1TenantSettingsResponse v1TenantSettingsResponse) {
        v.computeIfPresent(VOLUME, (key, value) -> convertToVolumeNumberFormat(value, v1TenantSettingsResponse));
        v.computeIfPresent(WEIGHT, (key, value) -> convertToWeightNumberFormat(value, v1TenantSettingsResponse));
        v.computeIfPresent(NET_WEIGHT, (key, value) -> convertToWeightNumberFormat(value, v1TenantSettingsResponse));
        v.computeIfPresent(VOLUME_WEIGHT, (key, value) -> convertToWeightNumberFormat(value.toString(), v1TenantSettingsResponse));
        v.computeIfPresent(PACKS, (key, value) -> getDPWWeightVolumeFormat(new BigDecimal(value.toString()), 0, v1TenantSettingsResponse));
    }

    private void addCarrierDetailTags(ShippingInstructionModel model, Map<String, Object> dictionary) {
        if (model.getShipment() != null && model.getShipment().getCarrierDetails() != null && model.getShipment().getCarrierDetails().getOrigin() != null) {
            var unlocRow = getUNLocRow(model.getShipment().getCarrierDetails().getOrigin());
            dictionary.put(POR, unlocRow != null ? unlocRow
                    .getNameWoDiacritics() : null);
        }
        if (model.getShipment() != null && model.getShipment().getCarrierDetails() != null) {
            dictionary.put(POL, getPortDetails(model.getShipment().getCarrierDetails().getOriginPort()));
            dictionary.put(POD, getPortDetails(model.getShipment().getCarrierDetails().getDestinationPort()));
            dictionary.put(POFD, getPortDetails(model.getShipment().getCarrierDetails().getDestination()));
            dictionary.put(PO_DELIVERY, getPortDetails(model.getShipment().getCarrierDetails().getDestinationPort()));
            String formatPattern = "dd/MMM/y";
            V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
            if(!CommonUtils.isStringNullOrEmpty(v1TenantSettingsResponse.getDPWDateFormat()))
                formatPattern = v1TenantSettingsResponse.getDPWDateFormat();
            dictionary.put(ETD, generateFormattedDate(model.getShipment().getCarrierDetails().getEtd(), formatPattern));
            dictionary.put(ETA, generateFormattedDate(model.getShipment().getCarrierDetails().getEta(), formatPattern));
            VesselsResponse vesselsResponse = getVesselsData(model.getShipment().getCarrierDetails().getVessel());
            if(vesselsResponse != null)
                dictionary.put(ReportConstants.VESSEL_NAME, vesselsResponse.getName());
            dictionary.put(VOYAGE, model.getShipment().getCarrierDetails().getVoyage());
            dictionary.put(FLIGHT_NUMBER, model.getShipment().getCarrierDetails().getFlightNumber());
        }
    }

    private void addNotifyPartyTags(ShippingInstructionModel model, Map<String, Object> dictionary) {
        PartiesModel notifyParty = model.getShipment().getAdditionalDetails() != null ? model.getShipment().getAdditionalDetails().getNotifyParty() : null;
        List<String> notify = getOrgAddressWithPhoneEmail(notifyParty);
        if (notifyParty != null && notifyParty.getOrgData() != null && notifyParty.getOrgData().get(FULL_NAME) != null) {
            notify.add(0, notifyParty.getOrgData().get(FULL_NAME).toString());
        }
        dictionary.put(NOTIFY_ADDRESS, notify);
        if (notifyParty != null && notifyParty.getAddressData() != null  && notifyParty.getAddressData().containsKey(PartiesConstants.RAW_DATA)) {
            dictionary.put(NOTIFY_PARTY_FREETEXT,
                    notifyParty.getAddressData().get(PartiesConstants.RAW_DATA));
        } else {
            dictionary.put(NOTIFY_PARTY_FREETEXT, notify);
        }
    }

    private void addConsigneeTags(ShippingInstructionModel model, Map<String, Object> dictionary) {
        PartiesModel consigneeParty = model.getShipment().getConsignee();
        List<String> consignee = getOrgAddressWithPhoneEmail(consigneeParty);
        if (consigneeParty != null && consigneeParty.getOrgData() != null && consigneeParty.getOrgData().get(FULL_NAME) != null) {
            consignee.add(0, consigneeParty.getOrgData().get(FULL_NAME).toString());
        }
        dictionary.put(CONSIGNEE_ADDRESS, consignee);
        if (consigneeParty != null && consigneeParty.getAddressData() != null  && consigneeParty.getAddressData().containsKey(PartiesConstants.RAW_DATA)) {
            dictionary.put(CONSIGNEE_FREETEXT,
                    consigneeParty.getAddressData().get(PartiesConstants.RAW_DATA));
        } else {
            dictionary.put(CONSIGNEE_FREETEXT, consignee);
        }
    }

    private void addConsignerTags(ShippingInstructionModel model, Map<String, Object> dictionary) {
        PartiesModel consignerParty = model.getShipment().getConsigner();
        List<String> consigner = getOrgAddressWithPhoneEmail(consignerParty);
        if (consignerParty != null && consignerParty.getOrgData() != null && consignerParty.getOrgData().get(FULL_NAME) != null) {
            consigner.add(0, consignerParty.getOrgData().get(FULL_NAME).toString());
        }
        dictionary.put(SHIPPER_ADDRESS, consigner);
        if (consignerParty != null && consignerParty.getAddressData() != null  && consignerParty.getAddressData().containsKey(PartiesConstants.RAW_DATA)) {
            dictionary.put(CONSIGNER_FREETEXT,
                    consignerParty.getAddressData().get(PartiesConstants.RAW_DATA));
        } else {
            dictionary.put(CONSIGNER_FREETEXT, consigner);
        }
    }

    public void addBlDetails(Map<String, Object> dictionary, Long shipmentId) {
        List<Hbl> hbl = hblRepository.findByShipmentId(shipmentId);
        if(hbl != null && !hbl.isEmpty()){
            dictionary.put(ReportConstants.BL_CARGO_TERMS_DESCRIPTION,
                    hbl.get(0).getHblData().getCargoTermsDescription());
            dictionary.put(ReportConstants.BL_REMARKS_DESCRIPTION,
                    hbl.get(0).getHblData().getBlRemarksDescription());
        }
    }

    private static class VolumeDetails {
        BigDecimal totalVolume;
        String unitOfTotalVolume;
        boolean breakFlagForVolume;

        VolumeDetails(BigDecimal totalVolume, String unitOfTotalVolume, boolean breakFlagForVolume) {
            this.totalVolume = totalVolume;
            this.unitOfTotalVolume = unitOfTotalVolume;
            this.breakFlagForVolume = breakFlagForVolume;
        }
    }

    private static class WeightDetails {
        BigDecimal totalWeight;
        String unitOfTotalWeight;
        boolean breakFlagForWeight;

        WeightDetails(BigDecimal totalWeight, String unitOfTotalWeight, boolean breakFlagForWeight) {
            this.totalWeight = totalWeight;
            this.unitOfTotalWeight = unitOfTotalWeight;
            this.breakFlagForWeight = breakFlagForWeight;
        }
    }
}
