package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.CargoManifestModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.*;

@Component
public class CargoManifestReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public Map<String, Object> getData(Long id) {
        CargoManifestModel cargoManifestModel = (CargoManifestModel) getDocumentModel(id);
        return populateDictionary(cargoManifestModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        CargoManifestModel cargoManifestModel = new CargoManifestModel();
        cargoManifestModel.shipmentDetails = getShipment(id);
        cargoManifestModel.tenantDetails = getTenant();
        cargoManifestModel.usersDto = UserContext.getUser();
        return cargoManifestModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        CargoManifestModel cargoManifestModel = (CargoManifestModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        addTenantDetails(dictionary, cargoManifestModel.tenantDetails);

        List<String> consigner = new ArrayList<>();
        if(cargoManifestModel.shipmentDetails.getConsigner() != null) {
            consigner = getOrgAddressWithPhoneEmail(cargoManifestModel.shipmentDetails.getConsigner());
            if(cargoManifestModel.shipmentDetails.getConsigner().getOrgData() != null) {
                Map<String, Object> partyOrg = cargoManifestModel.shipmentDetails.getConsigner().getOrgData();
                if(getValueFromMap(partyOrg, ReportConstants.FULL_NAME) != null) {
                    consigner.add(0, getValueFromMap(partyOrg, ReportConstants.FULL_NAME));
                }
            }
        }

        List<String> consignee = new ArrayList<>();
        if(cargoManifestModel.shipmentDetails.getConsignee() != null) {
            consignee = getOrgAddressWithPhoneEmail(cargoManifestModel.shipmentDetails.getConsignee());
            if(cargoManifestModel.shipmentDetails.getConsignee().getOrgData() != null) {
                Map<String, Object> partyOrg = cargoManifestModel.shipmentDetails.getConsignee().getOrgData();
                if(getValueFromMap(partyOrg, ReportConstants.FULL_NAME) != null) {
                    consignee.add(0, getValueFromMap(partyOrg, ReportConstants.FULL_NAME));
                }
            }
        }

        List<String> notify = new ArrayList<>();
        if(cargoManifestModel.shipmentDetails.getAdditionalDetails().getNotifyParty() != null) {
            notify = getOrgAddressWithPhoneEmail(cargoManifestModel.shipmentDetails.getAdditionalDetails().getNotifyParty());
            if(cargoManifestModel.shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgData() != null) {
                Map<String, Object> partyOrg = cargoManifestModel.shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgData();
                if(getValueFromMap(partyOrg, ReportConstants.FULL_NAME) != null) {
                    notify.add(0, getValueFromMap(partyOrg, ReportConstants.FULL_NAME));
                }
            }
        }

        List<String> tenantsDataList = getListOfStrings(cargoManifestModel.tenantDetails.tenantName, cargoManifestModel.tenantDetails.address1, cargoManifestModel.tenantDetails.address2,
                cargoManifestModel.tenantDetails.city, cargoManifestModel.tenantDetails.state, cargoManifestModel.tenantDetails.zipPostCode, cargoManifestModel.tenantDetails.country,
                cargoManifestModel.tenantDetails.email, cargoManifestModel.tenantDetails.websiteUrl, cargoManifestModel.tenantDetails.phone);
        if(tenantsDataList != null)
            dictionary.put(ReportConstants.TENANT, tenantsDataList);
        dictionary.put(ReportConstants.CONSIGNOR, consigner);
        dictionary.put(ReportConstants.CONSIGNEE_ADDRESS, consignee);
        dictionary.put(ReportConstants.NOTIFY_PARTY, notify);
        dictionary.put(ReportConstants.NOTIFY_PARTY_FREETEXT, notify);
        dictionary.put(ReportConstants.CONSIGNEE_FREETEXT, consignee);
        dictionary.put(ReportConstants.CONSIGNER_FREETEXT, consigner);
        dictionary.put(ReportConstants.MAWB_NO, cargoManifestModel.shipmentDetails.getMasterBill());
        dictionary.put(ReportConstants.HAWB_NO, cargoManifestModel.shipmentDetails.getHouseBill());
        dictionary.put(ReportConstants.SHIPMENT_NO, cargoManifestModel.shipmentDetails.getShipmentId());
        if(cargoManifestModel.shipmentDetails.getCarrierDetails().getOrigin() != null) {
            UnlocationsResponse origin = getUNLocRow(cargoManifestModel.shipmentDetails.getCarrierDetails().getOrigin());
            if(origin != null)
                dictionary.put(ReportConstants.POR, origin.getNameWoDiacritics());
        }
        dictionary.put(ReportConstants.POL, getPortDetails(cargoManifestModel.shipmentDetails.getCarrierDetails().getOriginPort()));
        dictionary.put(ReportConstants.POD, getPortDetails(cargoManifestModel.shipmentDetails.getCarrierDetails().getDestinationPort()));
        dictionary.put(ReportConstants.FPOD, getPortDetails(cargoManifestModel.shipmentDetails.getCarrierDetails().getDestination()));
        dictionary.put(ReportConstants.CURRENT_DATE, IReport.ConvertToDPWDateFormat(LocalDateTime.now()));
        if(cargoManifestModel.shipmentDetails.getCarrierDetails().getEtd() != null) {
            dictionary.put(ReportConstants.ETD_CAPS, IReport.ConvertToDPWDateFormat(cargoManifestModel.shipmentDetails.getCarrierDetails().getEtd()));
        }
        if(cargoManifestModel.shipmentDetails.getCarrierDetails().getEta() != null) {
            dictionary.put(ReportConstants.ETA_CAPS, IReport.ConvertToDPWDateFormat(cargoManifestModel.shipmentDetails.getCarrierDetails().getEta()));
        }
        dictionary.put(ReportConstants.FLIGHT_NAME, cargoManifestModel.shipmentDetails.getCarrierDetails().getShippingLine());
        dictionary.put(ReportConstants.FLIGHT_NUMBER, cargoManifestModel.shipmentDetails.getCarrierDetails().getFlightNumber());
        dictionary.put(ReportConstants.PP_CC, cargoManifestModel.shipmentDetails.getPaymentTerms());
        dictionary.put(ReportConstants.BOOKING_NO, cargoManifestModel.shipmentDetails.getBookingNumber());
        if(cargoManifestModel.shipmentDetails.getPackingList() != null && cargoManifestModel.shipmentDetails.getPackingList().size() > 0) {
            List<Map<String, Object>> packDictionary = new ArrayList<>();
            for (PackingModel pack : cargoManifestModel.shipmentDetails.getPackingList()) {
                String packJson = jsonHelper.convertToJson(pack);
                packDictionary.add(jsonHelper.convertJsonToMap(packJson));
            }
            packDictionary.forEach(v -> JsonDateFormat(v));
            dictionary.put(ReportConstants.ITEMS, packDictionary);
        }
        dictionary.put(ReportConstants.CMS_REMARKS, cargoManifestModel.shipmentDetails.getAdditionalTerms());
        dictionary.put(ReportConstants.USER_EMAIL, cargoManifestModel.usersDto.Email);
        dictionary.put(ReportConstants.DATE_TIME, LocalDateTime.now().format(DateTimeFormatter.ofPattern("dd/MMM/y hh:mm a")));
        return dictionary;
    }

}
