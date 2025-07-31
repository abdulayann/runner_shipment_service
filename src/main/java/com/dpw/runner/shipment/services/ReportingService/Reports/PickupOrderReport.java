package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGES_SMALL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGE_TYPE_CODE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGE_TYPE_DESCRIPTION_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNEE_ADDRESS_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNEE_NAME_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTACT_PERSON;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CY_NAME_ADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DESTINATION_AGENT_ADDRESS_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DESTINATION_AGENT_NAME_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FULL_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORIGIN_AGENT_ADDRESS_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORIGIN_AGENT_NAME_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPPED_ONBOARD_DATE_DDMMMYYYY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPPED_ONBOARD_TEXT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPPER_ADDRESS_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPPER_NAME_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddress;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.HblModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.PickUpOrderReportModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PickupDeliveryDetailsModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.utils.StringUtility;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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

        processPickupDetails(pickUpOrderReportModel, dictionary);
        processHblShipment(pickUpOrderReportModel, dictionary, orgWithoutTranslation);
        if(dictionary.containsKey(CHARGES_SMALL) && dictionary.get(CHARGES_SMALL) instanceof List){
            List<Map<String, Object>> values = (List<Map<String, Object>>)dictionary.get(CHARGES_SMALL);
            for (Map<String, Object> v: values) {
                if(v.containsKey(CHARGE_TYPE_CODE) && v.get(CHARGE_TYPE_CODE) != null) {
                    v.put(CHARGE_TYPE_DESCRIPTION_LL, getChargeTypeDescriptionLL((String)v.get(CHARGE_TYPE_CODE), chargeTypesWithoutTranslation));
                }
            }
        }

        populateUserFields(pickUpOrderReportModel.hblModel.getUser(), dictionary);
        populateTenantFields(dictionary, pickUpOrderReportModel.hblModel.getTenant());

        dictionary.put(ReportConstants.PRINT_USER, UserContext.getUser().getUsername());
        populateRaKcData(dictionary, pickUpOrderReportModel.hblModel.shipment);
        handleTranslationErrors(printWithoutTranslation, orgWithoutTranslation, chargeTypesWithoutTranslation);

        // Add Party Details in Caps
        ReportHelper.addPartyNameAndAddressInCaps(pickUpOrderReportModel.hblModel.shipment.getConsigner(), dictionary, SHIPPER_NAME_IN_CAPS, SHIPPER_ADDRESS_IN_CAPS);
        ReportHelper.addPartyNameAndAddressInCaps(pickUpOrderReportModel.hblModel.shipment.getConsignee(), dictionary, CONSIGNEE_NAME_IN_CAPS, CONSIGNEE_ADDRESS_IN_CAPS);
        ReportHelper.addPartyNameAndAddressInCaps(pickUpOrderReportModel.hblModel.shipment.getAdditionalDetails().getImportBroker(), dictionary, DESTINATION_AGENT_NAME_IN_CAPS, DESTINATION_AGENT_ADDRESS_IN_CAPS);
        ReportHelper.addPartyNameAndAddressInCaps(pickUpOrderReportModel.hblModel.shipment.getAdditionalDetails().getExportBroker(), dictionary, ORIGIN_AGENT_NAME_IN_CAPS, ORIGIN_AGENT_ADDRESS_IN_CAPS);

        dictionary.put(SHIPPED_ONBOARD_TEXT, pickUpOrderReportModel.hblModel.shipment.getAdditionalDetails().getShippedOnboardText().toUpperCase());
        dictionary.put(SHIPPED_ONBOARD_DATE_DDMMMYYYY, convertToDPWDateFormat(
                pickUpOrderReportModel.hblModel.shipment.getAdditionalDetails().getShippedOnboardDate(), "ddMMMyyyy".toUpperCase(), false));

        ReportHelper.addTenantDetails(dictionary, pickUpOrderReportModel.hblModel.tenant);
        if (pickUpOrderReportModel.hblModel.getShipment() != null) {
            this.populateShipmentReportData(dictionary, null, pickUpOrderReportModel.hblModel.getShipment() .getId());
            this.getContainerDetails(pickUpOrderReportModel.hblModel.getShipment(), dictionary);
            this.getPackingDetails(pickUpOrderReportModel.hblModel.getShipment(), dictionary);
        }

        if (pickUpOrderReportModel.hblModel.getConsolidation() != null) {
            this.populateConsolidationReportData(dictionary, null, pickUpOrderReportModel.hblModel.getConsolidation().getId());
        }

        return dictionary;
    }

    private void processHblShipment(PickUpOrderReportModel pickUpOrderReportModel, Map<String, Object> dictionary, List<String> orgWithoutTranslation) {
        if(!Objects.isNull(pickUpOrderReportModel.hblModel.shipment)) {
            populateShipmentOrganizationsLL(pickUpOrderReportModel.hblModel.shipment, dictionary, orgWithoutTranslation);
            var shipmentConsigner = pickUpOrderReportModel.hblModel.shipment.getConsigner();
            if(shipmentConsigner != null && shipmentConsigner.getAddressData() != null){
                Map<String, Object> consignerAddress = shipmentConsigner.getAddressData();
                var rawData = consignerAddress.containsKey(PartiesConstants.RAW_DATA) ? StringUtility.convertToString(consignerAddress.get(PartiesConstants.RAW_DATA)) : null;
                var consignorFreeText = ReportHelper.getAddressList(rawData);
                dictionary.put(ReportConstants.CONSIGNER_FREETEXT, consignorFreeText);
                dictionary.put(ReportConstants.CONSIGNER_ADDRESS_FREE_TEXT_IN_CAPS, consignorFreeText == null ? null : consignorFreeText.stream().map(StringUtility::toUpperCase).collect(Collectors.toList()));
                dictionary.put(ReportConstants.CONSIGNER_NAME_FREETEXT_INCAPS, consignorFreeText == null ? null : consignorFreeText.stream().map(StringUtility::toUpperCase).collect(Collectors.toList()));
            }
        }
    }

    private void processPickupDetails(PickUpOrderReportModel pickUpOrderReportModel, Map<String, Object> dictionary) {
        if (pickUpOrderReportModel.hblModel.shipment != null && pickUpOrderReportModel.hblModel.shipment.getPickupDetails() != null) {
            PickupDeliveryDetailsModel pickupDetails = pickUpOrderReportModel.hblModel.shipment.getPickupDetails();
            List<String> pickUpFrom = getOrgAddress(pickupDetails.getSourceDetail());
            dictionary.put(ReportConstants.PICKUP_FROM, pickUpFrom);

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
    }
}
