package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.AmountNumberFormatter;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.TransportOrderModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ReferenceNumbersConstants;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Component;

import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.GOODS_VALUE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPPED_ONBOARD_DATE_DDMMMYYYY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPPED_ONBOARD_TEXT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getFormattedAddress;

@Component
@Slf4j
public class TransportOrderReport extends IReport{

    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        TransportOrderModel cargoManifestModel = (TransportOrderModel) getDocumentModel(id);
        return populateDictionary(cargoManifestModel);
    }

    public Map<String, Object> getData(Long id, Long transportInstructionId) throws RunnerException {
        TransportOrderModel cargoManifestModel = (TransportOrderModel) getDocumentModel(id);
        cargoManifestModel.shipmentDetails.setTransportInstructionId(transportInstructionId);
        return populateDictionary(cargoManifestModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) throws RunnerException {
        TransportOrderModel transportOrderModel = new TransportOrderModel();
        transportOrderModel.shipmentDetails = getShipment(id);
        ShipmentSettingsDetails shipmentSettingsDetails = getCurrentShipmentSettings();
        Boolean countryAirCargoSecurity = shipmentSettingsDetails.getCountryAirCargoSecurity();
        if (Boolean.TRUE.equals(countryAirCargoSecurity)) {
            validateAirDGAndAirSecurityCheckShipments(transportOrderModel.shipmentDetails);
        }
        validateAirAndOceanDGCheck(transportOrderModel.shipmentDetails);
        return transportOrderModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        TransportOrderModel transportOrderModel = (TransportOrderModel) documentModel;
        ShipmentModel shipmentModel = transportOrderModel.shipmentDetails;
        Map<String, Object> dictionary = new HashMap<>();
        dictionary.put(ReportConstants.SHIPMENT_NUMBER, shipmentModel.getShipmentId());
        processTruckDriverDetailsTags(shipmentModel, dictionary);
        populateV3TruckDriverDetailsTags(shipmentModel, dictionary);
        List<String> unlocoRequests = this.createUnLocoRequestFromShipmentModel(shipmentModel);
        Map<String, UnlocationsResponse> unlocationsMap = masterDataUtils.getLocationData(new HashSet<>(unlocoRequests));
        UnlocationsResponse origin = unlocationsMap.get(shipmentModel.getCarrierDetails().getOrigin());
        UnlocationsResponse destination = unlocationsMap.get(shipmentModel.getCarrierDetails().getDestination());
        dictionary.put(ReportConstants.ORIGIN, origin != null ? origin.getName() : null);
        dictionary.put(ReportConstants.DESTINATION, destination != null ? destination.getName() : null);
        dictionary.put(ReportConstants.ETA_CAPS, convertToDPWDateFormat(shipmentModel.getCarrierDetails().getEta()));
        dictionary.put(ReportConstants.ETD_CAPS, convertToDPWDateFormat(shipmentModel.getCarrierDetails().getEtd()));
        processContainersListTags(shipmentModel, dictionary);
        processReferenceNumbersListTags(shipmentModel, dictionary);
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        dictionary.put(GOODS_VALUE, AmountNumberFormatter.format(shipmentModel.getGoodsValue(), UserContext.getUser().getCompanyCurrency(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.GOODS_VALUE_CURRENCY, shipmentModel.getGoodsValueCurrency());
        dictionary.put(ReportConstants.INSURANCE_VALUE, AmountNumberFormatter.format(shipmentModel.getInsuranceValue(), UserContext.getUser().getCompanyCurrency(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.INSURANCE_VALUE_CURRENCY, shipmentModel.getInsuranceValueCurrency());

        dictionary.put(SHIPPED_ONBOARD_TEXT, shipmentModel.getAdditionalDetails().getShippedOnboardText().toUpperCase());
        dictionary.put(SHIPPED_ONBOARD_DATE_DDMMMYYYY, convertToDPWDateFormat(
                shipmentModel.getAdditionalDetails().getShippedOnboardDate(), "ddMMMyyyy".toUpperCase(), false));

        if(shipmentModel.getFreightLocal() != null)
            dictionary.put(ReportConstants.FREIGHT_LOCAL, AmountNumberFormatter.format(shipmentModel.getFreightLocal(), UserContext.getUser().getCompanyCurrency(), v1TenantSettingsResponse));
        if(shipmentModel.getFreightLocalCurrency() != null && !shipmentModel.getFreightLocalCurrency().isEmpty())
            dictionary.put(ReportConstants.FREIGHT_LOCAL_CURRENCY, shipmentModel.getFreightLocalCurrency());
        if(shipmentModel.getFreightOverseas() != null)
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS, AmountNumberFormatter.format(shipmentModel.getFreightOverseas(), UserContext.getUser().getCompanyCurrency(), v1TenantSettingsResponse));
        if(shipmentModel.getFreightOverseasCurrency() != null && !shipmentModel.getFreightOverseasCurrency().isEmpty())
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS_CURRENCY, shipmentModel.getFreightOverseasCurrency());
        if(shipmentModel.getPickupDetails() != null && shipmentModel.getPickupDetails().getSourceDetail() != null) {
            PartiesModel pickup = shipmentModel.getPickupDetails().getSourceDetail();
            dictionary.put(ReportConstants.PICK_UP_ADDRESS, getFormattedAddress(pickup,true));
            dictionary.put(ReportConstants.PICKUP_CONTACT, getValueFromMap(pickup.getAddressData(), ReportConstants.CONTACT_PHONE));
        }
        if(shipmentModel.getDeliveryDetails() != null && shipmentModel.getDeliveryDetails().getDestinationDetail() != null) {
            PartiesModel delivery = shipmentModel.getDeliveryDetails().getDestinationDetail();
            dictionary.put(ReportConstants.DELIVERY_ADDRESS, getFormattedAddress(delivery,true));
            dictionary.put(ReportConstants.DELIVERY_CONTACT, getValueFromMap(delivery.getAddressData(), ReportConstants.CONTACT_PHONE));
        }
        processBrokerDetailTags(dictionary, shipmentModel);
        addFreightLocalTags(shipmentModel, dictionary, v1TenantSettingsResponse);
        if(shipmentModel.getTransportInstructionId() != null)
            addTransportInstructionTags(dictionary, shipmentModel);
        if(transportOrderModel.shipmentDetails != null) {
            this.populateShipmentReportData(dictionary, null, transportOrderModel.shipmentDetails.getId());
            this.getContainerDetails(transportOrderModel.shipmentDetails, dictionary);
            this.getPackingDetails(transportOrderModel.shipmentDetails, dictionary);
        }
        return dictionary;
    }

    private void addFreightLocalTags(ShipmentModel shipmentModel, Map<String, Object> dictionary, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(shipmentModel.getFreightLocal() != null)
            dictionary.put(ReportConstants.FREIGHT_LOCAL, AmountNumberFormatter.format(shipmentModel.getFreightLocal(), shipmentModel.getFreightLocalCurrency(), v1TenantSettingsResponse));
        if(shipmentModel.getFreightLocalCurrency() != null && !shipmentModel.getFreightLocalCurrency().isEmpty())
            dictionary.put(ReportConstants.FREIGHT_LOCAL_CURRENCY, shipmentModel.getFreightLocalCurrency());
        if(shipmentModel.getFreightOverseas() != null)
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS, AmountNumberFormatter.format(shipmentModel.getFreightOverseas(), shipmentModel.getFreightOverseasCurrency(), v1TenantSettingsResponse));
        if(shipmentModel.getFreightOverseasCurrency() != null && !shipmentModel.getFreightOverseasCurrency().isEmpty())
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS_CURRENCY, shipmentModel.getFreightOverseasCurrency());
    }

    private void processBrokerDetailTags(Map<String, Object> dictionary, ShipmentModel shipmentModel) {
        try { dictionary.put(ReportConstants.EXPORT_BROKER, getValueFromMap(shipmentModel.getPickupDetails().getBrokerDetail().getOrgData(), ReportConstants.FULL_NAME)); } catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
        try { dictionary.put(ReportConstants.EXPORT_BROKER_CONTACT, getValueFromMap(shipmentModel.getPickupDetails().getBrokerDetail().getAddressData(), ReportConstants.CONTACT_PHONE)); } catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
        try { dictionary.put(ReportConstants.IMPORT_BROKER, getValueFromMap(shipmentModel.getDeliveryDetails().getBrokerDetail().getOrgData(), ReportConstants.FULL_NAME)); } catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
        try { dictionary.put(ReportConstants.IMPORT_BROKER_CONTACT, getValueFromMap(shipmentModel.getDeliveryDetails().getBrokerDetail().getAddressData(), ReportConstants.CONTACT_PHONE)); } catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
    }

    private void processReferenceNumbersListTags(ShipmentModel shipmentModel, Map<String, Object> dictionary) {
        if(shipmentModel.getReferenceNumbersList() != null && !shipmentModel.getReferenceNumbersList().isEmpty()) {
            for (ReferenceNumbersModel referenceNumbersModel: shipmentModel.getReferenceNumbersList()) {
                if(Objects.equals(referenceNumbersModel.getType(), ReferenceNumbersConstants.REF_NUM_TYPE_ETN))
                    dictionary.computeIfAbsent(ReportConstants.ENTRY_NUMBER, k -> referenceNumbersModel.getReferenceNumber());
                if(Objects.equals(referenceNumbersModel.getType(), ReferenceNumbersConstants.REF_NUM_TYPE_CRR))
                    dictionary.computeIfAbsent(ReportConstants.CUSTOMER_REFERENCE, k -> referenceNumbersModel.getReferenceNumber());
            }
        }
    }

    private void processContainersListTags(ShipmentModel shipmentModel, Map<String, Object> dictionary) {
        if(shipmentModel.getContainersList() != null && !shipmentModel.getContainersList().isEmpty()) {
            StringBuilder containerNumbers = null;
            StringBuilder carrierSealNumbers = null;
            for (ContainerModel containerModel: shipmentModel.getContainersList()) {
                containerNumbers = getContainerNumbers(containerModel, containerNumbers);
                carrierSealNumbers = getCarrierSealNumbers(containerModel, carrierSealNumbers);
            }
            if(carrierSealNumbers != null) dictionary.put(ReportConstants.CARRIER_SEAL_NUMBER, carrierSealNumbers.toString());
            if(containerNumbers != null) dictionary.put(ReportConstants.CONTAINER_NUM, containerNumbers.toString());
        }
    }

    private StringBuilder getCarrierSealNumbers(ContainerModel containerModel, StringBuilder carrierSealNumbers) {
        if(!CommonUtils.isStringNullOrEmpty(containerModel.getCarrierSealNumber())) {
            if(carrierSealNumbers == null)
                carrierSealNumbers = new StringBuilder(containerModel.getCarrierSealNumber());
            else
                carrierSealNumbers.append(", ").append(containerModel.getCarrierSealNumber());
        }
        return carrierSealNumbers;
    }

    private StringBuilder getContainerNumbers(ContainerModel containerModel, StringBuilder containerNumbers) {
        if(!CommonUtils.isStringNullOrEmpty(containerModel.getContainerNumber())) {
            if(containerNumbers == null)
                containerNumbers = new StringBuilder(containerModel.getContainerNumber());
            else
                containerNumbers.append(", ").append(containerModel.getContainerNumber());
        }
        return containerNumbers;
    }

    private void processTruckDriverDetailsTags(ShipmentModel shipmentModel, Map<String, Object> dictionary) {
        if(shipmentModel.getTruckDriverDetails() != null && !shipmentModel.getTruckDriverDetails().isEmpty()) {
            TruckDriverDetailsModel truckDriverDetailsModel = shipmentModel.getTruckDriverDetails().get(0);
            dictionary.put(ReportConstants.TRUCK_NUMBER_PLATE, truckDriverDetailsModel.getTruckNumberPlate());
            dictionary.put(ReportConstants.DRIVER_NAME, truckDriverDetailsModel.getDriverName());
            dictionary.put(ReportConstants.TRAILER_NUMBER_PLATE, truckDriverDetailsModel.getTrailerNumberPlate());
            dictionary.put(ReportConstants.DRIVER_MOBILE_NUMBER, truckDriverDetailsModel.getDriverMobileNumber());
            if(truckDriverDetailsModel.getTransporterType().equals(Ownership.Self)) {
                dictionary.put(ReportConstants.TRANSPORTER_NAME, truckDriverDetailsModel.getSelfTransporterName());
            } else {
                try { dictionary.put(ReportConstants.TRANSPORTER_NAME, truckDriverDetailsModel.getThirdPartyTransporter().getOrgData().get(ReportConstants.FULL_NAME)); } catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
            }
        }
    }
}
