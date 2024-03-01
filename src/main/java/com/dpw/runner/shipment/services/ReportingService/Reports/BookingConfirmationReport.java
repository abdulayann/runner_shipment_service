package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.BookingConfirmationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.BookingCarriageModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ReferenceNumbersModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.ReferenceNumbersConstants;
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

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;

@Component
public class BookingConfirmationReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private HblReport hblReport;

    private Long id;

    @Override
    public Map<String, Object> getData(Long id) {
        BookingConfirmationModel bookingConfirmationModel = (BookingConfirmationModel) getDocumentModel(id);
        this.id = id;
        return populateDictionary(bookingConfirmationModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        BookingConfirmationModel bookingConfirmationModel = new BookingConfirmationModel();
        bookingConfirmationModel.shipment = getShipment(id);
        bookingConfirmationModel.user = UserContext.getUser();
        if(bookingConfirmationModel.shipment != null && bookingConfirmationModel.shipment.getConsolidationList() != null && !bookingConfirmationModel.shipment.getConsolidationList().isEmpty())
        {
            bookingConfirmationModel.consolidation = bookingConfirmationModel.shipment.getConsolidationList().get(0);
        }
        bookingConfirmationModel.blObject = getHbl(id);
        bookingConfirmationModel.commonContainers = new ArrayList<>();
        if(bookingConfirmationModel.shipment.getContainersList() != null)
        {
            for(ContainerModel container : bookingConfirmationModel.shipment.getContainersList())
            {
                ShipmentContainers shipmentContainer = getShipmentContainer(container);
                shipmentContainer.BL_SealNumber = container.getCustomsSealNumber();
                bookingConfirmationModel.commonContainers.add(shipmentContainer);
            }
        }

        Set<String> locCodes = new HashSet<>();
        if (bookingConfirmationModel.shipment.getCarrierDetails() != null && bookingConfirmationModel.shipment.getCarrierDetails().getOriginPort() != null) {
            locCodes.add(bookingConfirmationModel.shipment.getCarrierDetails().getOriginPort());
        }

        if (bookingConfirmationModel.shipment.getCarrierDetails() != null && bookingConfirmationModel.shipment.getCarrierDetails().getDestinationPort() != null) {
            locCodes.add(bookingConfirmationModel.shipment.getCarrierDetails().getDestinationPort());
        }

        if (bookingConfirmationModel.shipment.getCarrierDetails() != null && bookingConfirmationModel.shipment.getCarrierDetails().getDestinationPort() != null) {
            locCodes.add(bookingConfirmationModel.shipment.getAdditionalDetails().getPaidPlace());
        }

        Map<String, UnlocationsResponse> unlocationMap = masterDataUtils.getLocationData(locCodes);

        UnlocationsResponse paidPlace = null;
        UnlocationsResponse location = null;
        if (bookingConfirmationModel.shipment.getAdditionalDetails() != null &&
                unlocationMap.get(bookingConfirmationModel.shipment.getAdditionalDetails().getPaidPlace()) != null) {
            paidPlace = unlocationMap.get(bookingConfirmationModel.shipment.getAdditionalDetails().getPaidPlace());
        }
        location = bookingConfirmationModel.shipment.getCarrierDetails() != null ?
                unlocationMap.get(bookingConfirmationModel.shipment.getCarrierDetails().getOriginPort()) : null;
        if (location != null) {
            bookingConfirmationModel.polName = location.getName();
            bookingConfirmationModel.polCountry = location.getCountry();
        }
        location = bookingConfirmationModel.shipment.getCarrierDetails() != null ?
                unlocationMap.get(bookingConfirmationModel.shipment.getCarrierDetails().getDestinationPort()) : null;
        if (location != null) {
            bookingConfirmationModel.podName = location.getName();
            bookingConfirmationModel.podCountry = location.getCountry();
        }

        bookingConfirmationModel.setReferenceNumbersList(bookingConfirmationModel.shipment.getReferenceNumbersList());
        MasterData masterData = getMasterListData(MasterDataType.PAYMENT, bookingConfirmationModel.shipment.getPaymentTerms());
        bookingConfirmationModel.paymentTerms = (masterData != null ? masterData.getItemDescription() : null);
        masterData = getMasterListData(MasterDataType.SERVICE_MODE, bookingConfirmationModel.shipment.getServiceType());
        bookingConfirmationModel.serviceMode = (masterData != null ? masterData.getItemDescription() : null);

        if(paidPlace != null) {
            masterData = getMasterListData(MasterDataType.COUNTRIES, paidPlace.getCountry());
        }
        bookingConfirmationModel.paidPlaceCountry = (masterData != null ? masterData.getItemDescription() : null);
        List<BookingCarriageModel> bookingCarriages = bookingConfirmationModel.shipment.getBookingCarriagesList();
        BookingCarriageModel bookingCarriage = null;
        if(bookingCarriages != null)
        {
            for (BookingCarriageModel carriage : bookingCarriages) {
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

        Map<String, Object> dictionary = hblReport.getData(this.id);

        BookingConfirmationModel bookingConfirmationModel = (BookingConfirmationModel) documentModel;
        populateShipmentOrganizationsLL(bookingConfirmationModel.shipment, dictionary);
        if(dictionary.containsKey(CHARGES_SMALL) && dictionary.get(CHARGES_SMALL) instanceof List){
            List<Map<String, Object>> values = (List<Map<String, Object>>)dictionary.get(CHARGES_SMALL);
            for (Map<String, Object> v: values) {
                if(v.containsKey(CHARGE_TYPE_CODE) && v.get(CHARGE_TYPE_CODE) != null) {
                    v.put(CHARGE_TYPE_DESCRIPTION_LL, GetChargeTypeDescriptionLL((String)v.get(CHARGE_TYPE_CODE)));
                }
            }
        }

        dictionary.put(ReportConstants.HAWB_NO, bookingConfirmationModel.shipment.getHouseBill());
        dictionary.put(ReportConstants.MAWB_NO, bookingConfirmationModel.shipment.getMasterBill());
        dictionary.put(ReportConstants.PORT_OF_DEPARTURE, bookingConfirmationModel.polName);
        if (bookingConfirmationModel.polName != null)
            dictionary.put(ReportConstants.PortofDepartureInCaps, bookingConfirmationModel.polName.toUpperCase());
        dictionary.put(ReportConstants.PORT_OF_DEPARTURE_COUNTRY, bookingConfirmationModel.polCountry);
        if (bookingConfirmationModel.polCountry != null)
            dictionary.put(ReportConstants.SHIPMENT_DETAILS_PORTOFDEPARTURECOUNTRYINCAPS, bookingConfirmationModel.polCountry.toUpperCase());
        dictionary.put(ReportConstants.PORT_OF_ARRIVAL, bookingConfirmationModel.podName);
        if (bookingConfirmationModel.podName != null)
            dictionary.put(ReportConstants.PortofArrivalInCaps, bookingConfirmationModel.podName.toUpperCase());
        dictionary.put(ReportConstants.PORT_OF_ARRIVAL_COUNTRY, bookingConfirmationModel.podCountry);
        if (bookingConfirmationModel.podCountry != null)
            dictionary.put(ReportConstants.SHIPMENT_DETAILS_PORTOFARRIVALCOUNTRYINCAPS, bookingConfirmationModel.podCountry.toUpperCase());

        dictionary.put(ReportConstants.MOVEMENT_TYPE, bookingConfirmationModel.shipment.getTransportMode());

        List<ReferenceNumbersModel> referenceNumbers = bookingConfirmationModel.getReferenceNumbersList();

        if (referenceNumbers != null && referenceNumbers.size() > 0) {
            List<ReferenceNumbersModel> conditionBasedReferenceNo = new ArrayList<>();
            List<ReferenceNumbersModel> motherReferenceNo = new ArrayList<>();
            List<ReferenceNumbersModel> feederReferenceNo = new ArrayList<>();
            for (ReferenceNumbersModel refNo : referenceNumbers) {
                if (refNo != null && refNo.getType() != null && refNo.getType().equalsIgnoreCase("BKG")) {
                    dictionary.put(ReportConstants.BOOKING_NUMBER, refNo.getReferenceNumber());
                    break;
                }
            }
            for (ReferenceNumbersModel refNo : referenceNumbers) {
                if (refNo != null && refNo.getType() != null &&
                        refNo.getType().equalsIgnoreCase(ReferenceNumbersConstants.FEEDER_VESSEL) ||
                        refNo.getType().equalsIgnoreCase(ReferenceNumbersConstants.MOTHER_VESSEL)) {
                    if (refNo.getType().equalsIgnoreCase(ReferenceNumbersConstants.MOTHER_VESSEL)) {
                        motherReferenceNo.add(refNo);
                    } else if (refNo.getType().equalsIgnoreCase(ReferenceNumbersConstants.FEEDER_VESSEL)) {
                        feederReferenceNo.add(refNo);
                    }
                    conditionBasedReferenceNo.add(refNo);
                }
            }
            dictionary.put(ReportConstants.BOOKING_NO_BASED_ON_TYPE, conditionBasedReferenceNo);

            dictionary.put(ReportConstants.MOTHER_REFERENCE_NO, motherReferenceNo);
            dictionary.put(ReportConstants.FEEDER_REFERENCE_NO, feederReferenceNo);
        }

        dictionary.put(ReportConstants.PAYMENT, bookingConfirmationModel.shipment.getPaymentTerms());

        return dictionary;
    }
}
