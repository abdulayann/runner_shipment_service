package com.dpw.runner.shipment.services.reportingservice.Reports;

import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.reportingservice.Models.HblModel;
import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.reportingservice.Models.PickUpOrderReportModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.PickupDeliveryDetailsModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.reportingservice.CommonUtils.ReportHelper.getOrgAddress;

@Component
public class PickupOrderReport extends IReport {

    @Autowired
    private HblReport hblReport;
    public Boolean printWithoutTranslation;

    @Override
    public Map<String, Object> getData(Long id) {
        PickUpOrderReportModel pickUpOrderReportModel = (PickUpOrderReportModel) getDocumentModel(id);
        return populateDictionary(pickUpOrderReportModel);
    }

    public Map<String, Object> getData(Long id, Long transportInstructionId) {
        PickUpOrderReportModel pickUpOrderReportModel = (PickUpOrderReportModel) getDocumentModel(id);
        pickUpOrderReportModel.hblModel.transportInstructionId = transportInstructionId;
        return populateDictionary(pickUpOrderReportModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {
        PickUpOrderReportModel pickUpOrderReportModel = new PickUpOrderReportModel();
        pickUpOrderReportModel.hblModel = (HblModel) hblReport.getDocumentModel(id);
        pickUpOrderReportModel.hblModel.isHbl = false;
        if (pickUpOrderReportModel.hblModel.shipment != null && pickUpOrderReportModel.hblModel.shipment.getPickupDetails() != null)
            pickUpOrderReportModel.pickUpTransportAddress = pickUpOrderReportModel.hblModel.shipment.getPickupDetails().getTransporterDetail();
        ShipmentSettingsDetails shipmentSettingsDetails = getCurrentShipmentSettings();
        Boolean countryAirCargoSecurity = shipmentSettingsDetails.getCountryAirCargoSecurity();
        if (Boolean.TRUE.equals(countryAirCargoSecurity)) {
            validateAirDGAndAirSecurityCheckShipments(pickUpOrderReportModel.hblModel.getShipment());
        } else {
            validateAirDGCheckShipments(pickUpOrderReportModel.hblModel.getShipment());
        }
        return pickUpOrderReportModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        PickUpOrderReportModel pickUpOrderReportModel = (PickUpOrderReportModel) documentModel;
        List<String> orgWithoutTranslation = new ArrayList<>();
        List<String> chargeTypesWithoutTranslation = new ArrayList<>();
        Map<String, Object> dictionary = hblReport.populateDictionary(pickUpOrderReportModel.hblModel);
        if(pickUpOrderReportModel.pickUpTransportAddress != null && pickUpOrderReportModel.pickUpTransportAddress.getAddressData() != null)
            dictionary.put(ReportConstants.PICKUP_TRANSPORT_CONTACT_PERSON, pickUpOrderReportModel.pickUpTransportAddress.getAddressData().get("ContactPerson"));

        if (pickUpOrderReportModel.hblModel.shipment != null && pickUpOrderReportModel.hblModel.shipment.getPickupDetails() != null) {
            PickupDeliveryDetailsModel pickupDetails = pickUpOrderReportModel.hblModel.shipment.getPickupDetails();
            List<String> pickUpFrom = getOrgAddress(pickupDetails.getSourceDetail());
            dictionary.put(ReportConstants.PICK_UP_FROM, pickUpFrom);

            // P0 tags pickup order doc
            if(pickupDetails.getTransporterDetail() != null) {
                dictionary.put(ReportConstants.PICKUP_TRANSPORT_COMPANY, getValueFromMap(pickupDetails.getTransporterDetail().getOrgData(), ReportConstants.FULL_NAME));
                dictionary.put(ReportConstants.PICKUP_TRANSPORT_CONTACT_PERSON, getValueFromMap(pickupDetails.getTransporterDetail().getAddressData(), CONTACT_PERSON));
            }
            if(pickupDetails.getSourceDetail() != null) {
                dictionary.put(ReportConstants.PICKUP_COMPANY, getValueFromMap(pickupDetails.getSourceDetail().getOrgData(), ReportConstants.FULL_NAME));
            }

            if(pickUpOrderReportModel.hblModel.shipment.getPickupDetails().getDestinationDetail() != null) {
                List<String> cyNameAddress = new ArrayList<>();
                if(!Boolean.TRUE.equals(pickUpOrderReportModel.hblModel.shipmentSettingsDetails.getDisableBlPartiesName()))
                    cyNameAddress.add(getValueFromMap(pickUpOrderReportModel.hblModel.shipment.getPickupDetails().getDestinationDetail().getOrgData(), FULL_NAME));
                cyNameAddress.addAll(getOrgAddress(pickUpOrderReportModel.hblModel.shipment.getPickupDetails().getDestinationDetail()));
                dictionary.put(CY_NAME_ADDRESS, String.join("\r\n", cyNameAddress));
            }
        }
        if(!Objects.isNull(pickUpOrderReportModel.hblModel.shipment)) {
            populateShipmentOrganizationsLL(pickUpOrderReportModel.hblModel.shipment, dictionary, orgWithoutTranslation);
            var shipmentConsigner = pickUpOrderReportModel.hblModel.shipment.getConsigner();
            if(shipmentConsigner != null && shipmentConsigner.getAddressData() != null){
                Map<String, Object> consignerAddress = shipmentConsigner.getAddressData();
                var rawData = consignerAddress != null && consignerAddress.containsKey(PartiesConstants.RAW_DATA) ? StringUtility.convertToString(consignerAddress.get(PartiesConstants.RAW_DATA)) : null;
                var consignorFreeText = ReportHelper.getAddressList(rawData);
                dictionary.put(ReportConstants.CONSIGNER_FREETEXT, consignorFreeText);
                dictionary.put(ReportConstants.CONSIGNER_ADDRESS_FREE_TEXT_IN_CAPS, consignorFreeText == null ? null : consignorFreeText.stream().map(StringUtility::toUpperCase).collect(Collectors.toList()));
                dictionary.put(ReportConstants.CONSIGNER_NAME_FREETEXT_INCAPS, consignorFreeText == null ? null : consignorFreeText.stream().map(StringUtility::toUpperCase).collect(Collectors.toList()));
            }
        }
        if(dictionary.containsKey(CHARGES_SMALL) && dictionary.get(CHARGES_SMALL) instanceof List){
            List<Map<String, Object>> values = (List<Map<String, Object>>)dictionary.get(CHARGES_SMALL);
            for (Map<String, Object> v: values) {
                if(v.containsKey(CHARGE_TYPE_CODE) && v.get(CHARGE_TYPE_CODE) != null) {
                    v.put(CHARGE_TYPE_DESCRIPTION_LL, GetChargeTypeDescriptionLL((String)v.get(CHARGE_TYPE_CODE), chargeTypesWithoutTranslation));
                }
            }
        }

        populateUserFields(pickUpOrderReportModel.hblModel.getUser(), dictionary);
        populateTenantFields(dictionary, pickUpOrderReportModel.hblModel.getTenant());

        dictionary.put(ReportConstants.PRINT_USER, UserContext.getUser().getUsername());
        populateRaKcData(dictionary, pickUpOrderReportModel.hblModel.shipment);
        HandleTranslationErrors(printWithoutTranslation, orgWithoutTranslation, chargeTypesWithoutTranslation);

        return dictionary;
    }
}
