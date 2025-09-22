package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGES_SMALL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGE_TYPE_CODE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGE_TYPE_DESCRIPTION_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNEE_ADDRESS_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNEE_NAME_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DESTINATION_AGENT_ADDRESS_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DESTINATION_AGENT_NAME_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORIGIN_AGENT_ADDRESS_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORIGIN_AGENT_NAME_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPPER_ADDRESS_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPPER_NAME_IN_CAPS;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.BookingConfirmationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.HblModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ReferenceNumbersModel;
import com.dpw.runner.shipment.services.commons.constants.ReferenceNumbersConstants;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class BookingConfirmationReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private HblReport hblReport;

    public Boolean printWithoutTranslation;

    @Override
    public Map<String, Object> getData(Long id) {
        BookingConfirmationModel bookingConfirmationModel = (BookingConfirmationModel) getDocumentModel(id);
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
        populateChargeDescriptions(dictionary, chargeTypesWithoutTranslation);

        dictionary.put(ReportConstants.HAWB_NO, bookingConfirmationModel.hblModel.shipment.getHouseBill());
        dictionary.put(ReportConstants.MAWB_NO, bookingConfirmationModel.hblModel.shipment.getMasterBill());
        dictionary.put(ReportConstants.PORT_OF_DEPARTURE, bookingConfirmationModel.hblModel.polName);
        if (bookingConfirmationModel.hblModel.polName != null)
            dictionary.put(ReportConstants.PORTOF_DEPARTURE_IN_CAPS, bookingConfirmationModel.hblModel.polName.toUpperCase());
        dictionary.put(ReportConstants.PORT_OF_DEPARTURE_COUNTRY, bookingConfirmationModel.hblModel.polCountry);
        if (bookingConfirmationModel.hblModel.polCountry != null)
            dictionary.put(ReportConstants.SHIPMENT_DETAILS_PORTOFDEPARTURECOUNTRYINCAPS, bookingConfirmationModel.hblModel.polCountry.toUpperCase());
        dictionary.put(ReportConstants.PORT_OF_ARRIVAL, bookingConfirmationModel.hblModel.podName);
        if (bookingConfirmationModel.hblModel.podName != null)
            dictionary.put(ReportConstants.PORTOF_ARRIVAL_IN_CAPS, bookingConfirmationModel.hblModel.podName.toUpperCase());
        dictionary.put(ReportConstants.PORT_OF_ARRIVAL_COUNTRY, bookingConfirmationModel.hblModel.podCountry);
        if (bookingConfirmationModel.hblModel.podCountry != null)
            dictionary.put(ReportConstants.SHIPMENT_DETAILS_PORTOFARRIVALCOUNTRYINCAPS, bookingConfirmationModel.hblModel.podCountry.toUpperCase());

        dictionary.put(ReportConstants.MOVEMENT_TYPE, bookingConfirmationModel.hblModel.shipment.getTransportMode());

        populateShippedOnboardFields(bookingConfirmationModel.hblModel.shipment, dictionary);

        List<ReferenceNumbersModel> referenceNumbers = bookingConfirmationModel.getReferenceNumbersList();

        if (referenceNumbers != null && !referenceNumbers.isEmpty()) {
            processReferenceNumberList(referenceNumbers, dictionary);
        }

        dictionary.put(ReportConstants.PAYMENT, bookingConfirmationModel.hblModel.shipment.getPaymentTerms());
        handleTranslationErrors(printWithoutTranslation, orgWithoutTranslation, chargeTypesWithoutTranslation);
        dictionary.put(ReportConstants.CARRIER_BOOKING_NUMBER, bookingConfirmationModel.hblModel.shipment.getBookingNumber());

        ReportHelper.addPartyNameAndAddressInCaps(bookingConfirmationModel.hblModel.shipment.getConsigner(), dictionary, SHIPPER_NAME_IN_CAPS, SHIPPER_ADDRESS_IN_CAPS);
        ReportHelper.addPartyNameAndAddressInCaps(bookingConfirmationModel.hblModel.shipment.getConsignee(), dictionary, CONSIGNEE_NAME_IN_CAPS, CONSIGNEE_ADDRESS_IN_CAPS);
        ReportHelper.addPartyNameAndAddressInCaps(bookingConfirmationModel.hblModel.shipment.getAdditionalDetails().getImportBroker(), dictionary, DESTINATION_AGENT_NAME_IN_CAPS, DESTINATION_AGENT_ADDRESS_IN_CAPS);
        ReportHelper.addPartyNameAndAddressInCaps(bookingConfirmationModel.hblModel.shipment.getAdditionalDetails().getExportBroker(), dictionary, ORIGIN_AGENT_NAME_IN_CAPS, ORIGIN_AGENT_ADDRESS_IN_CAPS);

        ReportHelper.addTenantDetails(dictionary, bookingConfirmationModel.hblModel.tenant);

        if (bookingConfirmationModel.hblModel.consolidation != null) {
            this.populateConsolidationReportData(dictionary, null, bookingConfirmationModel.hblModel.consolidation.getId());
        }

        if(bookingConfirmationModel.hblModel.shipment != null) {
            this.populateShipmentReportData(dictionary, null, bookingConfirmationModel.hblModel.shipment.getId());
            this.getContainerDetails(bookingConfirmationModel.hblModel.getShipment(), dictionary);
            this.getPackingDetails(bookingConfirmationModel.hblModel.getShipment(), dictionary);
        }
        return dictionary;
    }

    private void populateChargeDescriptions(Map<String, Object> dictionary, List<String> chargeTypesWithoutTranslation) {
        if(dictionary.containsKey(CHARGES_SMALL) && dictionary.get(CHARGES_SMALL) instanceof List) {
            List<Map<String, Object>> values = (List<Map<String, Object>>) dictionary.get(CHARGES_SMALL);
            for (Map<String, Object> charge: values) {
                if(charge.containsKey(CHARGE_TYPE_CODE) && charge.get(CHARGE_TYPE_CODE) != null) {
                    charge.put(CHARGE_TYPE_DESCRIPTION_LL, getChargeTypeDescriptionLL((String)charge.get(CHARGE_TYPE_CODE), chargeTypesWithoutTranslation));
                }
            }
        }
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
