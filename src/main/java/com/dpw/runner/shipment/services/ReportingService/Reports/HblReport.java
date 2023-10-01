package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.HblModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.BookingCarriageModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PickupDeliveryDetailsModel;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;

@Component
public class HblReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IV1Service v1Service;

    @Override
    public Map<String, Object> getData(Long id) {
        HblModel hblModel = (HblModel) getDocumentModel(id);
        return populateDictionary(hblModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        HblModel hblModel = new HblModel();
        hblModel.shipment = getShipment(id);
        if(hblModel.shipment != null && hblModel.shipment.getConsolidationList() != null && !hblModel.shipment.getConsolidationList().isEmpty())
        {
            hblModel.consolidation = hblModel.shipment.getConsolidationList().get(0);
        }
        Map<String, HblContainerDto> hblContainerDtoMap = new HashMap<>();
        hblModel.blObject = getHbl(id);
        hblModel.commonContainers = new ArrayList<>();
        if(hblModel.blObject.getHblContainer() != null)
        {
            for(HblContainerDto hblContainerDto : hblModel.blObject.getHblContainer())
            {
                hblContainerDtoMap.put(hblContainerDto.getContainerNumber(), hblContainerDto);
            }
        }
        if(hblModel.shipment.getContainersList() != null)
        {
            for(ContainerModel container : hblModel.shipment.getContainersList())
            {
                ShipmentContainers shipmentContainer = getShipmentContainer(container);
                if(hblContainerDtoMap.containsKey(container.getContainerNumber()))
                {
                    populateBLContainer(shipmentContainer, hblContainerDtoMap.get(container.getContainerNumber()));
                }
                shipmentContainer.BL_SealNumber = container.getCustomsSealNumber();
                hblModel.commonContainers.add(shipmentContainer);
            }
        }
        MasterData masterData = getMasterListData(MasterDataType.PAYMENT, hblModel.shipment.getPaymentTerms());
        hblModel.paymentTerms = (masterData != null ? masterData.getItemDescription() : null);
        masterData = getMasterListData(MasterDataType.SERVICE_MODE, hblModel.shipment.getServiceType());
        hblModel.serviceMode = (masterData != null ? masterData.getItemDescription() : null);
        UnlocationsResponse paidPlace = null;
        List<Object> criteria = Arrays.asList(
                Arrays.asList("LocCode"),
                "=",
                hblModel.shipment.getAdditionalDetails().getPaidPlace()
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
        V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
        List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
        if(unlocationsResponse != null && unlocationsResponse.size() > 0)
            paidPlace = unlocationsResponse.get(0);
        masterData = getMasterListData(MasterDataType.COUNTRIES, paidPlace.getCountry());
        hblModel.paidPlaceCountry = (masterData != null ? masterData.getItemDescription() : null);
        List<BookingCarriageModel> bookingCarriages = hblModel.shipment.getBookingCarriagesList();
        BookingCarriageModel bookingCarriage = null;
        if(bookingCarriages != null)
        {
            for(int i=0; i<bookingCarriages.size(); i++)
            {
                if(Objects.equals(bookingCarriages.get(i).getCarriageType(), "PreCarriage"))
                {
                    bookingCarriage = bookingCarriages.get(i);
                    break;
                }
            }
        }
        if(bookingCarriage != null)
        {
            String vessel = bookingCarriage.getVessel();
            List<Object> vesselCriteria = Arrays.asList(
                    Arrays.asList("Mmsi"),
                    "=",
                    vessel
            );
            CommonV1ListRequest vesselRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(vesselCriteria).build();
            V1DataResponse vesselResponse = v1Service.fetchVesselData(vesselRequest);
            List<VesselsResponse> vesselsResponse = jsonHelper.convertValueToList(vesselResponse.entities, VesselsResponse.class);
            if(vesselsResponse != null && vesselsResponse.size() > 0)
                hblModel.preCarriageVessel = vesselsResponse.get(0);
        }
        return hblModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        HblModel hblModel = (HblModel) documentModel;
        String json = jsonHelper.convertToJson(hblModel.shipment);
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        populateShipmentFields(hblModel.shipment, false, dictionary);
        populateConsolidationFields(hblModel.consolidation, dictionary);
        populateBlFields(hblModel.blObject, dictionary);
        dictionary.put(ReportConstants.PAID_PLACE_COUNTRY_NAME, hblModel.paidPlaceCountry);
        dictionary.put(ReportConstants.SERVICE_MODE_DESCRIPTION, hblModel.serviceMode);
        dictionary.put(ReportConstants.PPCC, hblModel.paymentTerms);
        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(hblModel.commonContainers));
        dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, hblModel.commonContainers);
        dictionary.put(ReportConstants.PRE_CARRIAGE, hblModel.preCarriageVessel != null ? hblModel.preCarriageVessel.getName() : null);
        PickupDeliveryDetailsModel pickup = hblModel.shipment.getPickupDetails();
        if(pickup != null && pickup.getTransporterDetail() != null)
        {
            Map<String, Object> address = pickup.getTransporterDetail().getAddressData();
            dictionary.put(ReportConstants.PICKUP_TRANSPORT, ReportHelper.getOrgAddressWithPhoneEmail(
                    address.get("OrgFullName").toString(), address.get(ReportConstants.ADDRESS1).toString(), address.get(ReportConstants.ADDRESS2).toString(),
                    address.get(ReportConstants.COUNTRY).toString(), address.get(ReportConstants.EMAIL).toString(), address.get(ReportConstants.CONTACT_PHONE).toString(),
                            null
            ));
        }
        PickupDeliveryDetailsModel delivery = hblModel.shipment.getDeliveryDetails();
        if(delivery != null && delivery.getAgentDetail() != null)
        {
            Map<String, Object> address = delivery.getAgentDetail().getAddressData();
            dictionary.put(ReportConstants.DELIVERY_AGENT, ReportHelper.getOrgAddressWithPhoneEmail(
                    address.get("OrgFullName").toString(), address.get(ReportConstants.ADDRESS1).toString(), address.get(ReportConstants.ADDRESS2).toString(),
                    address.get(ReportConstants.COUNTRY).toString(), address.get(ReportConstants.EMAIL).toString(), address.get(ReportConstants.CONTACT_PHONE).toString(),
                    null
            ));
        }
        return dictionary;
    }
}
