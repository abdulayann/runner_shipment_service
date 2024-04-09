package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.AmountNumberFormatter;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.ReportingService.Models.TransportOrderModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.ReferenceNumbersConstants;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.GOODS_VALUE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddress;
import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;

@Component
public class TransportOrderReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        TransportOrderModel cargoManifestModel = (TransportOrderModel) getDocumentModel(id);
        return populateDictionary(cargoManifestModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) throws RunnerException {
        TransportOrderModel transportOrderModel = new TransportOrderModel();
        transportOrderModel.shipmentDetails = getShipment(id);
        return transportOrderModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        TransportOrderModel transportOrderModel = (TransportOrderModel) documentModel;
        ShipmentModel shipmentModel = transportOrderModel.shipmentDetails;
        Map<String, Object> dictionary = new HashMap<>();
        dictionary.put(ReportConstants.SHIPMENT_NUMBER, shipmentModel.getShipmentId());
        if(shipmentModel.getTruckDriverDetails() != null && !shipmentModel.getTruckDriverDetails().isEmpty()) {
            TruckDriverDetailsModel truckDriverDetailsModel = shipmentModel.getTruckDriverDetails().get(0);
            dictionary.put(ReportConstants.TRUCK_NUMBER_PLATE, truckDriverDetailsModel.getTruckNumberPlate());
            dictionary.put(ReportConstants.DRIVER_NAME, truckDriverDetailsModel.getDriverName());
            dictionary.put(ReportConstants.TRAILER_NUMBER_PLATE, truckDriverDetailsModel.getTrailerNumberPlate());
            dictionary.put(ReportConstants.DRIVER_MOBILE_NUMBER, truckDriverDetailsModel.getDriverMobileNumber());
            if(truckDriverDetailsModel.getTransporterType().equals(Ownership.Self)) {
                dictionary.put(ReportConstants.TRANSPORTER_NAME, truckDriverDetailsModel.getSelfTransporterName());
            } else {
                try { dictionary.put(ReportConstants.TRANSPORTER_NAME, truckDriverDetailsModel.getThirdPartyTransporter().getOrgData().get(ReportConstants.FULL_NAME)); } catch (Exception ignored) {}
            }
        }
        List<String> unlocoRequests = this.createUnLocoRequestFromShipmentModel(shipmentModel);
        Map<String, UnlocationsResponse> unlocationsMap = masterDataUtils.getLocationData(new HashSet<>(unlocoRequests));
        UnlocationsResponse origin = unlocationsMap.get(shipmentModel.getCarrierDetails().getOrigin());
        UnlocationsResponse destination = unlocationsMap.get(shipmentModel.getCarrierDetails().getDestination());
        dictionary.put(ReportConstants.ORIGIN, origin != null ? origin.getName() : null);
        dictionary.put(ReportConstants.DESTINATION, destination != null ? destination.getName() : null);
        dictionary.put(ReportConstants.ETA_CAPS, ConvertToDPWDateFormat(shipmentModel.getCarrierDetails().getEta()));
        dictionary.put(ReportConstants.ETD_CAPS, ConvertToDPWDateFormat(shipmentModel.getCarrierDetails().getEtd()));
        if(shipmentModel.getContainersList() != null && !shipmentModel.getContainersList().isEmpty()) {
            StringBuilder containerNumbers = null;
            StringBuilder carrierSealNumbers = null;
            for (ContainerModel containerModel: shipmentModel.getContainersList()) {
                if(!IsStringNullOrEmpty(containerModel.getContainerNumber())) {
                    if(containerNumbers == null)
                        containerNumbers = new StringBuilder(containerModel.getContainerNumber());
                    else
                        containerNumbers.append(", ").append(containerModel.getContainerNumber());
                }
                if(!IsStringNullOrEmpty(containerModel.getCarrierSealNumber())) {
                    if(carrierSealNumbers == null)
                        carrierSealNumbers = new StringBuilder(containerModel.getCarrierSealNumber());
                    else
                        carrierSealNumbers.append(", ").append(containerModel.getCarrierSealNumber());
                }
            }
            if(carrierSealNumbers != null) dictionary.put(ReportConstants.CARRIER_SEAL_NUMBER, carrierSealNumbers.toString());
            if(containerNumbers != null) dictionary.put(ReportConstants.CONTAINER_NUM, containerNumbers.toString());
        }
        if(shipmentModel.getReferenceNumbersList() != null && !shipmentModel.getReferenceNumbersList().isEmpty()) {
            for (ReferenceNumbersModel referenceNumbersModel: shipmentModel.getReferenceNumbersList()) {
                if(Objects.equals(referenceNumbersModel.getType(), ReferenceNumbersConstants.REF_NUM_TYPE_ETN) && !dictionary.containsKey(ReportConstants.ENTRY_NUMBER))
                    dictionary.put(ReportConstants.ENTRY_NUMBER, referenceNumbersModel.getReferenceNumber());
                if(Objects.equals(referenceNumbersModel.getType(), ReferenceNumbersConstants.REF_NUM_TYPE_CRR) && !dictionary.containsKey(ReportConstants.CUSTOMER_REFERENCE))
                    dictionary.put(ReportConstants.CUSTOMER_REFERENCE, referenceNumbersModel.getReferenceNumber());
            }
        }
        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        dictionary.put(GOODS_VALUE, AmountNumberFormatter.Format(shipmentModel.getGoodsValue(), UserContext.getUser().getCompanyCurrency(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.GOODS_VALUE_CURRENCY, shipmentModel.getGoodsValueCurrency());
        dictionary.put(ReportConstants.INSURANCE_VALUE_TRANSPORT, AmountNumberFormatter.Format(shipmentModel.getInsuranceValue(), UserContext.getUser().getCompanyCurrency(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.INSURANCE_VALUE_CURRENCY, shipmentModel.getInsuranceValueCurrency());
        if(shipmentModel.getPickupDetails() != null && shipmentModel.getPickupDetails().getSourceDetail() != null) {
            PartiesModel pickup = shipmentModel.getPickupDetails().getSourceDetail();
            dictionary.put(ReportConstants.PICK_UP_ADDRESS, getOrgAddress(pickup));
            dictionary.put(ReportConstants.PICKUP_CONTACT, getValueFromMap(pickup.getAddressData(), ReportConstants.CONTACT_PHONE));
        }
        if(shipmentModel.getDeliveryDetails() != null && shipmentModel.getDeliveryDetails().getDestinationDetail() != null) {
            PartiesModel delivery = shipmentModel.getDeliveryDetails().getDestinationDetail();
            dictionary.put(ReportConstants.DELIVERY_ADDRESS, getOrgAddress(delivery));
            dictionary.put(ReportConstants.DELIVERY_CONTACT, getValueFromMap(delivery.getAddressData(), ReportConstants.CONTACT_PHONE));
        }
        try { dictionary.put(ReportConstants.EXPORT_BROKER, getValueFromMap(shipmentModel.getPickupDetails().getBrokerDetail().getOrgData(), ReportConstants.FULL_NAME)); } catch (Exception ignored) {}
        try { dictionary.put(ReportConstants.IMPORT_BROKER, getValueFromMap(shipmentModel.getDeliveryDetails().getBrokerDetail().getOrgData(), ReportConstants.FULL_NAME)); } catch (Exception ignored) {}
        return dictionary;
    }
}
