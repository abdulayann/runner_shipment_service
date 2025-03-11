package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.BookingConfirmationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.HblModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ReferenceNumbersModel;
import com.dpw.runner.shipment.services.commons.constants.ReferenceNumbersConstants;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
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
    public Boolean printWithoutTranslation;

    @Override
    public Map<String, Object> getData(Long id) {
        BookingConfirmationModel bookingConfirmationModel = (BookingConfirmationModel) getDocumentModel(id);
        this.id = id;
        return populateDictionary(bookingConfirmationModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        BookingConfirmationModel bookingConfirmationModel = new BookingConfirmationModel();
        bookingConfirmationModel.hblModel = (HblModel) hblReport.getDocumentModel(id);
        bookingConfirmationModel.hblModel.isHbl = false;
        bookingConfirmationModel.setReferenceNumbersList(bookingConfirmationModel.hblModel.shipment.getReferenceNumbersList());
        return bookingConfirmationModel;

    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {

        BookingConfirmationModel bookingConfirmationModel = (BookingConfirmationModel) documentModel;
        Map<String, Object> dictionary = hblReport.populateDictionary(bookingConfirmationModel.hblModel);
        List<String> orgWithoutTranslation = new ArrayList<>();
        List<String> chargeTypesWithoutTranslation = new ArrayList<>();

        populateShipmentOrganizationsLL(bookingConfirmationModel.hblModel.shipment, dictionary, orgWithoutTranslation);
        if(dictionary.containsKey(CHARGES_SMALL) && dictionary.get(CHARGES_SMALL) instanceof List){
            List<Map<String, Object>> values = (List<Map<String, Object>>) dictionary.get(CHARGES_SMALL);
            for (Map<String, Object> v: values) {
                if(v.containsKey(CHARGE_TYPE_CODE) && v.get(CHARGE_TYPE_CODE) != null) {
                    v.put(CHARGE_TYPE_DESCRIPTION_LL, GetChargeTypeDescriptionLL((String)v.get(CHARGE_TYPE_CODE), chargeTypesWithoutTranslation));
                }
            }
        }

        dictionary.put(ReportConstants.HAWB_NO, bookingConfirmationModel.hblModel.shipment.getHouseBill());
        dictionary.put(ReportConstants.MAWB_NO, bookingConfirmationModel.hblModel.shipment.getMasterBill());
        dictionary.put(ReportConstants.PORT_OF_DEPARTURE, bookingConfirmationModel.hblModel.polName);
        if (bookingConfirmationModel.hblModel.polName != null)
            dictionary.put(ReportConstants.PortofDepartureInCaps, bookingConfirmationModel.hblModel.polName.toUpperCase());
        dictionary.put(ReportConstants.PORT_OF_DEPARTURE_COUNTRY, bookingConfirmationModel.hblModel.polCountry);
        if (bookingConfirmationModel.hblModel.polCountry != null)
            dictionary.put(ReportConstants.SHIPMENT_DETAILS_PORTOFDEPARTURECOUNTRYINCAPS, bookingConfirmationModel.hblModel.polCountry.toUpperCase());
        dictionary.put(ReportConstants.PORT_OF_ARRIVAL, bookingConfirmationModel.hblModel.podName);
        if (bookingConfirmationModel.hblModel.podName != null)
            dictionary.put(ReportConstants.PortofArrivalInCaps, bookingConfirmationModel.hblModel.podName.toUpperCase());
        dictionary.put(ReportConstants.PORT_OF_ARRIVAL_COUNTRY, bookingConfirmationModel.hblModel.podCountry);
        if (bookingConfirmationModel.hblModel.podCountry != null)
            dictionary.put(ReportConstants.SHIPMENT_DETAILS_PORTOFARRIVALCOUNTRYINCAPS, bookingConfirmationModel.hblModel.podCountry.toUpperCase());

        dictionary.put(ReportConstants.MOVEMENT_TYPE, bookingConfirmationModel.hblModel.shipment.getTransportMode());

        List<ReferenceNumbersModel> referenceNumbers = bookingConfirmationModel.getReferenceNumbersList();

        if (referenceNumbers != null && !referenceNumbers.isEmpty()) {
            processReferenceNumberList(referenceNumbers, dictionary);
        }

        dictionary.put(ReportConstants.PAYMENT, bookingConfirmationModel.hblModel.shipment.getPaymentTerms());
        HandleTranslationErrors(printWithoutTranslation, orgWithoutTranslation, chargeTypesWithoutTranslation);

        return dictionary;
    }

    private void processReferenceNumberList(List<ReferenceNumbersModel> referenceNumbers, Map<String, Object> dictionary) {
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
                    (refNo.getType().equalsIgnoreCase(ReferenceNumbersConstants.FEEDER_VESSEL) ||
                    refNo.getType().equalsIgnoreCase(ReferenceNumbersConstants.MOTHER_VESSEL))) {
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
}
