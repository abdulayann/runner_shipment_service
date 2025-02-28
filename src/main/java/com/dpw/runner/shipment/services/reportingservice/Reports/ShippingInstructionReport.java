package com.dpw.runner.shipment.services.reportingservice.Reports;

import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShippingInstructionModel;
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

import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportHelper.*;
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

        PartiesModel consignerParty = model.getShipment().getConsigner();
        PartiesModel consigneeParty = model.getShipment().getConsignee();
        PartiesModel notifyParty = model.getShipment().getAdditionalDetails() != null ? model.getShipment().getAdditionalDetails().getNotifyParty() : null;

        List<String> consigner = getOrgAddressWithPhoneEmail(consignerParty);
        if (consignerParty != null && consignerParty.getOrgData() != null && consignerParty.getOrgData().get(FULL_NAME) != null) {
            consigner.add(0, consignerParty.getOrgData().get(FULL_NAME).toString());
        }
        List<String> consignee = getOrgAddressWithPhoneEmail(consigneeParty);
        if (consigneeParty != null && consigneeParty.getOrgData() != null && consigneeParty.getOrgData().get(FULL_NAME) != null) {
            consignee.add(0, consigneeParty.getOrgData().get(FULL_NAME).toString());
        }
        List<String> notify = getOrgAddressWithPhoneEmail(notifyParty);
        if (notifyParty != null && notifyParty.getOrgData() != null && notifyParty.getOrgData().get(FULL_NAME) != null) {
            notify.add(0, notifyParty.getOrgData().get(FULL_NAME).toString());
        }

        dictionary.put(SHIPPER_ADDRESS, consigner);
        dictionary.put(CONSIGNEE_ADDRESS, consignee);
        dictionary.put(NOTIFY_ADDRESS, notify);

        if (consignerParty != null && consignerParty.getAddressData() != null  && consignerParty.getAddressData().containsKey(PartiesConstants.RAW_DATA)) {
            dictionary.put(CONSIGNER_FREETEXT,
                    consignerParty.getAddressData().get(PartiesConstants.RAW_DATA));
        } else {
            dictionary.put(CONSIGNER_FREETEXT, consigner);
        }

        if (consigneeParty != null && consigneeParty.getAddressData() != null  && consigneeParty.getAddressData().containsKey(PartiesConstants.RAW_DATA)) {
            dictionary.put(CONSIGNEE_FREETEXT,
                    consigneeParty.getAddressData().get(PartiesConstants.RAW_DATA));
        } else {
            dictionary.put(CONSIGNEE_FREETEXT, consignee);
        }

        if (notifyParty != null && notifyParty.getAddressData() != null  && notifyParty.getAddressData().containsKey(PartiesConstants.RAW_DATA)) {
            dictionary.put(NOTIFY_PARTY_FREETEXT,
                    notifyParty.getAddressData().get(PartiesConstants.RAW_DATA));
        } else {
            dictionary.put(NOTIFY_PARTY_FREETEXT, notify);
        }

        dictionary.put(MBL_NUMBER, model.getShipment().getMasterBill());
        dictionary.put(HBL_NUMBER, model.getShipment().getHouseBill());
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
            if(!CommonUtils.IsStringNullOrEmpty(v1TenantSettingsResponse.getDPWDateFormat()))
                formatPattern = v1TenantSettingsResponse.getDPWDateFormat();
            dictionary.put(ETD, GenerateFormattedDate(model.getShipment().getCarrierDetails().getEtd(), formatPattern));
            dictionary.put(ETA, GenerateFormattedDate(model.getShipment().getCarrierDetails().getEta(), formatPattern));
            VesselsResponse vesselsResponse = getVesselsData(model.getShipment().getCarrierDetails().getVessel());
            if(vesselsResponse != null)
                dictionary.put(ReportConstants.VESSEL_NAME, vesselsResponse.getName());
            dictionary.put(VOYAGE, model.getShipment().getCarrierDetails().getVoyage());
            dictionary.put(FLIGHT_NUMBER, model.getShipment().getCarrierDetails().getFlightNumber());
        }
        dictionary.put(SERVICE_TYPE, model.getShipment().getTransportMode());
        dictionary.put(PPCC, model.getShipment().getPaymentTerms());
        dictionary.put(CURRENT_DATE, ConvertToDPWDateFormat(LocalDateTime.now()));


        dictionary.put(JOB_NUMBER, model.getShipment().getShipmentId());
        dictionary.put(TRANSPORT_MODE, model.getShipment().getTransportMode());
        dictionary.put(SHIPMENT_TYPE, model.getShipment().getShipmentType());

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

                if (!breakFlagForVolume && v.get(VOLUME) != null && v.get(VOLUME_UNIT) != null) {
                    if (unitOfTotalVolume == null) {
                        unitOfTotalVolume = v.get(VOLUME_UNIT).toString();
                        totalVolume = totalVolume.add(BigDecimal.valueOf(Double.parseDouble(StringUtility.convertToString(v.get(VOLUME)))));
                    } else if (!unitOfTotalVolume.equals(v.get(VOLUME_UNIT).toString())) {
                        totalVolume = BigDecimal.ZERO;
                        breakFlagForVolume = true;
                    } else
                        totalVolume = totalVolume.add(BigDecimal.valueOf(Double.parseDouble(StringUtility.convertToString(v.get(VOLUME)))));
                }

                if (!breakFlagForWeight && v.get(WEIGHT) != null && v.get(WEIGHT_UNIT) != null)
                {
                    if(unitOfTotalWeight == null) {
                        unitOfTotalWeight = v.get(WEIGHT_UNIT).toString();
                        totalWeight = totalWeight.add(BigDecimal.valueOf(Double.valueOf(v.get(WEIGHT).toString())));
                    }
                    else if(!unitOfTotalWeight.equals(v.get(WEIGHT_UNIT).toString())) {
                        totalWeight = BigDecimal.ZERO;
                        breakFlagForWeight = true;
                    }
                    else
                        totalWeight = totalWeight.add(BigDecimal.valueOf(Double.valueOf(v.get(WEIGHT).toString())));
                }

                if(v.get(VOLUME) != null)
                    v.put(VOLUME, ConvertToVolumeNumberFormat(v.get(VOLUME), v1TenantSettingsResponse));
                if(v.get(WEIGHT) != null)
                    v.put(WEIGHT, ConvertToWeightNumberFormat(v.get(WEIGHT), v1TenantSettingsResponse));
                if(v.get(NET_WEIGHT) != null)
                    v.put(NET_WEIGHT, ConvertToWeightNumberFormat(v.get(NET_WEIGHT), v1TenantSettingsResponse));
                if(v.get(VOLUME_WEIGHT) != null)
                    v.put(VOLUME_WEIGHT, ConvertToWeightNumberFormat(v.get(VOLUME_WEIGHT).toString(), v1TenantSettingsResponse));
                if(v.get(PACKS) != null)
                    v.put(PACKS, GetDPWWeightVolumeFormat(new BigDecimal(v.get(PACKS).toString()), 0, v1TenantSettingsResponse));
            }

            dictionary.put(ITEMS ,values);
        }

        if(totalPacks != 0)
            dictionary.put(TOTAL_PACKS, GetDPWWeightVolumeFormat(new BigDecimal(totalPacks), 0, v1TenantSettingsResponse));
        else
            dictionary.put(TOTAL_PACKS, null);

        if (breakFlagForVolume || totalVolume.equals(BigDecimal.ZERO)) {
            dictionary.put(TOTAL_PACKS_VOLUME, null);
            dictionary.put(UOTV, null);
        } else {
            dictionary.put(TOTAL_PACKS_VOLUME, ConvertToVolumeNumberFormat(totalVolume, v1TenantSettingsResponse));
            dictionary.put(UOTV, unitOfTotalVolume);
        }

        if (breakFlagForWeight || totalWeight.equals(BigDecimal.ZERO)) {
            dictionary.put(TOTAL_PACKS_WEIGHT, null);
            dictionary.put(UOTW, null);
        } else {
            dictionary.put(TOTAL_PACKS_WEIGHT, ConvertToWeightNumberFormat(totalWeight, v1TenantSettingsResponse));
            dictionary.put(UOTW, unitOfTotalWeight);
        }

        var tenant = model.getTenant();
        List<String> tenantDetails = ReportHelper.getOrgAddress(tenant.tenantName, tenant.address1, tenant.address2,
                tenant.country, tenant.zipPostCode, tenant.state);
        dictionary.put(AGENT, tenantDetails);
        ReportHelper.addTenantDetails(dictionary, model.getTenant());
        dictionary.put(TENANT, ReportHelper.getListOfStrings(tenant.tenantName, tenant.address1, tenant.address2,
                        tenant.city, tenant.state, tenant.zipPostCode, tenant.country,tenant.email, tenant.websiteUrl, tenant.phone));


        //Add Bl-details
        AddBlDetails(dictionary, model.getShipment().getId());

        return dictionary;
    }

    public void AddBlDetails(Map<String, Object> dictionary, Long shipmentId) {
        List<Hbl> hbl = hblRepository.findByShipmentId(shipmentId);
        if(hbl != null && hbl.size() > 0){
            dictionary.put(ReportConstants.BL_CARGO_TERMS_DESCRIPTION,
                    hbl.get(0).getHblData().getCargoTermsDescription());
            dictionary.put(ReportConstants.BL_REMARKS_DESCRIPTION,
                    hbl.get(0).getHblData().getBlRemarksDescription());
        }
    }
}
