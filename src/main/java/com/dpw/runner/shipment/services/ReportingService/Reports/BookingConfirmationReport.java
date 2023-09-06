package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.BookingConfirmationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.commons.constants.ReferenceNumbersConstants;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.BookingCarriage;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
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
public class BookingConfirmationReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private HblReport hblReport;

    @Override
    public Map<String, Object> getData(Long id) {
        BookingConfirmationModel bookingConfirmationModel = (BookingConfirmationModel) getDocumentModel(id);
        return populateDictionary(bookingConfirmationModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        BookingConfirmationModel bookingConfirmationModel = new BookingConfirmationModel();
        bookingConfirmationModel.shipment = getShipment(id);
        if(bookingConfirmationModel.shipment != null && bookingConfirmationModel.shipment.getConsolidationList() != null && !bookingConfirmationModel.shipment.getConsolidationList().isEmpty())
        {
            bookingConfirmationModel.consolidation = bookingConfirmationModel.shipment.getConsolidationList().get(0);
        }
        bookingConfirmationModel.blObject = getHbl(id);
        bookingConfirmationModel.commonContainers = new ArrayList<>();
        if(bookingConfirmationModel.shipment.getContainersList() != null)
        {
            for(Containers container : bookingConfirmationModel.shipment.getContainersList())
            {
                ShipmentContainers shipmentContainer = getShipmentContainer(container);
                shipmentContainer.BL_SealNumber = container.getCustomsSealNumber();
                bookingConfirmationModel.commonContainers.add(shipmentContainer);
            }
        }

        Set<String> locCodes = new HashSet<>();
        if(bookingConfirmationModel.shipment.getCarrierDetails().getOriginPort()!=null){
            locCodes.add(bookingConfirmationModel.shipment.getCarrierDetails().getOriginPort());
        }
        if(bookingConfirmationModel.shipment.getCarrierDetails().getDestinationPort()!=null){
            locCodes.add(bookingConfirmationModel.shipment.getCarrierDetails().getDestinationPort());
        }

        if(bookingConfirmationModel.shipment.getCarrierDetails().getDestinationPort()!=null){
            locCodes.add(bookingConfirmationModel.shipment.getAdditionalDetails().getPaidPlace());
        }

        Map<String, UnlocationsResponse> unlocationMap = getLocationData(locCodes);

        UnlocationsResponse paidPlace = null;
        UnlocationsResponse location;
        if(unlocationMap.get(bookingConfirmationModel.shipment.getAdditionalDetails().getPaidPlace()) !=null) {
            paidPlace = unlocationMap.get(bookingConfirmationModel.shipment.getAdditionalDetails().getPaidPlace());
        }
        location = unlocationMap.get(bookingConfirmationModel.shipment.getCarrierDetails().getOriginPort());
        if(location != null) {
            bookingConfirmationModel.polName = location.getName();
            bookingConfirmationModel.polCountry = location.getCountry();
        }
        location = unlocationMap.get(bookingConfirmationModel.shipment.getCarrierDetails().getDestinationPort());
        if(location != null) {
            bookingConfirmationModel.podName = location.getName();
            bookingConfirmationModel.podCountry = location.getCountry();
        }

        bookingConfirmationModel.referenceNumbersList = bookingConfirmationModel.shipment.getReferenceNumbersList();
        MasterData masterData = getMasterListData(MasterDataType.PAYMENT, bookingConfirmationModel.shipment.getPaymentTerms());
        bookingConfirmationModel.paymentTerms = (masterData != null ? masterData.getItemDescription() : null);
        masterData = getMasterListData(MasterDataType.SERVICE_MODE, bookingConfirmationModel.shipment.getServiceType());
        bookingConfirmationModel.serviceMode = (masterData != null ? masterData.getItemDescription() : null);

        if(paidPlace != null) {
            masterData = getMasterListData(MasterDataType.COUNTRIES, paidPlace.getCountry());
        }
        bookingConfirmationModel.paidPlaceCountry = (masterData != null ? masterData.getItemDescription() : null);
        List<BookingCarriage> bookingCarriages = bookingConfirmationModel.shipment.getBookingCarriagesList();
        BookingCarriage bookingCarriage = null;
        if(bookingCarriages != null)
        {
            for (BookingCarriage carriage : bookingCarriages) {
                if (Objects.equals(carriage.getCarriageType(), "PreCarriage")) {
                    bookingCarriage = carriage;
                    break;
                }
            }
        }
        if(bookingCarriage != null)
        {
            String vessel = bookingCarriage.getVessel();
            List<Object> vesselCriteria = Arrays.asList(
                    List.of("Mmsi"),
                    "=",
                    vessel
            );
            CommonV1ListRequest vesselRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(vesselCriteria).build();
            V1DataResponse vesselResponse = v1Service.fetchVesselData(vesselRequest);
            List<VesselsResponse> vesselsResponse = jsonHelper.convertValueToList(vesselResponse.entities, VesselsResponse.class);
            if(vesselsResponse != null && vesselsResponse.size() > 0)
                bookingConfirmationModel.preCarriageVessel = vesselsResponse.get(0);
        }
        return bookingConfirmationModel;

    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {

        Map<String, Object> dictionary = hblReport.populateDictionary(documentModel);

        BookingConfirmationModel bookingConfirmationModel = (BookingConfirmationModel) documentModel;

        dictionary.put(ReportConstants.HAWB_NO, bookingConfirmationModel.shipment.getHouseBill());
        dictionary.put(ReportConstants.MAWB_NO, bookingConfirmationModel.shipment.getMasterBill());
        dictionary.put(ReportConstants.PORT_OF_DEPARTURE, bookingConfirmationModel.polName);
        dictionary.put(ReportConstants.PORT_OF_DEPARTURE_COUNTRY, bookingConfirmationModel.polCountry);
        dictionary.put(ReportConstants.PORT_OF_ARRIVAL, bookingConfirmationModel.podName);
        dictionary.put(ReportConstants.PORT_OF_ARRIVAL_COUNTRY, bookingConfirmationModel.podCountry);

        dictionary.put(ReportConstants.MOVEMENT_TYPE, bookingConfirmationModel.shipment.getTransportMode());

        List<ReferenceNumbers> referenceNumbers = bookingConfirmationModel.referenceNumbersList;

        if(referenceNumbers != null && referenceNumbers.size() > 0)
        {
            List<ReferenceNumbers> conditionBasedReferenceNo = new ArrayList<>();
            List<ReferenceNumbers> motherReferenceNo = new ArrayList<>();
            List<ReferenceNumbers> feederReferenceNo = new ArrayList<>();
            for(ReferenceNumbers refNo : referenceNumbers)
            {
                if(refNo!=null && refNo.getType().equalsIgnoreCase("BKG"))
                {
                    dictionary.put(ReportConstants.BOOKING_NUMBER, refNo.getReferenceNumber());
                    break;
                }
            }
            for(ReferenceNumbers refNo : referenceNumbers)
            {
                if(refNo.getType().equalsIgnoreCase(ReferenceNumbersConstants.FEEDER_VESSEL) || refNo.getType().equalsIgnoreCase(ReferenceNumbersConstants.MOTHER_VESSEL)){
                    if(refNo.getType().equalsIgnoreCase(ReferenceNumbersConstants.MOTHER_VESSEL)){
                        motherReferenceNo.add(refNo);
                    }
                    else if( refNo.getType().equalsIgnoreCase(ReferenceNumbersConstants.FEEDER_VESSEL)){
                        feederReferenceNo.add(refNo);
                    }
                    conditionBasedReferenceNo.add(refNo);
                }
            }
            dictionary.put(ReportConstants.BOOKING_NO_BASED_ON_TYPE, conditionBasedReferenceNo);

            dictionary.put(ReportConstants.MOTHER_REFERENCE_NO, motherReferenceNo);
            dictionary.put(ReportConstants.FEEDER_REFERENCE_NO, feederReferenceNo);
        }

        dictionary.put(ReportConstants.PAYMENTS, bookingConfirmationModel.shipment.getPaymentTerms());

        return dictionary;
    }

    private Map<String, UnlocationsResponse> getLocationData(Set<String> locCodes) {
        Map<String, UnlocationsResponse> locationMap = new HashMap<>();
        if (locCodes.size() > 0) {
            List<Object> criteria = Arrays.asList(
                    List.of("LocCode"),
                    "In",
                    List.of(locCodes)
            );
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
            V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
            List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
            if (unlocationsResponse != null && unlocationsResponse.size() > 0) {
                for (UnlocationsResponse unlocation : unlocationsResponse) {
                    locationMap.put(unlocation.getLocCode(), unlocation);
                }
            }
        }
        return locationMap;
    }
}
