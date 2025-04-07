package com.dpw.runner.shipment.services.ReportingService;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Reports.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;

@Component
@SuppressWarnings("java:S6539")
public class ReportsFactory {

    private final Map<String, IReport> reportsMap;

    @Autowired
    public ReportsFactory(ArrivalNoticeReport arrivalNoticeReport,
                          ShippingInstructionReport shippingInstructionReport,
                          BookingConfirmationReport bookingConfirmationReport,
                          DeliveryOrderReport deliveryOrderReport,
                          HblReport hblReport,
                          HawbReport hawbReport,
                          MawbReport mawbReport,
                          CargoManifestReport cargoManifestReport,
                          CommercialInvoiceReport commercialInvoiceReport,
                          ConsolidatedPackingListReport consolidatedPackingListReport,
                          FreightCertificationReport freightCertificationReport,
                          ManifestConsolReport manifestConsolReport,
                          ManifestShipmentReport manifestShipmentReport,
                          AWBLabelReport awbLabelReport,
                          ShippingRequestAirReport shippingRequestAirReport,
                          PickupOrderReport pickupOrderReport,
                          ProofOfDeliveryReport proofOfDeliveryReport,
                          PackingListReport packingListReport,
                          ShipmentCANReport shipmentCANReport,
                          CustomsInstructionsReport customsInstructionsReport,
                          PreAlertReport preAlertReport,
                          ManifestPrintReport manifestPrintReport,
                          ShippingRequestOutReport shippingRequestOutReport,
                          ContainerManifestPrint containerManifestPrint,
                          SeawayBillReport seawayBillReport,
                          ShipTruckwayBillReport shipTruckwayBillReport,
                          ConsTruckwayBillReport consTruckwayBillReport,
                          ShipTruckDriverProof shipTruckDriverProof,
                          ConsTruckDriverProof consTruckDriverProof,
                          TransportOrderReport transportOrderReport,
                          CargoManifestAirShipmentReport cargoManifestAirShipmentReport,
                          CargoManifestAirConsolidationReport cargoManifestAirConsolidationReport,
                          BookingOrderReport bookingOrderReport,
                          CSDReport csdReport,
                          FCRDocumentReport fcrDocumentReport) {
        reportsMap = new HashMap<>();
        reportsMap.put(ReportConstants.ARRIVAL_NOTICE, arrivalNoticeReport);
        reportsMap.put(ReportConstants.SHIPPING_INSTRUCTION, shippingInstructionReport);
        reportsMap.put(ReportConstants.BOOKING_CONFIRMATION, bookingConfirmationReport);
        reportsMap.put(ReportConstants.DELIVERY_ORDER, deliveryOrderReport);
        reportsMap.put(ReportConstants.HOUSE_BILL, hblReport);
        reportsMap.put(ReportConstants.HAWB, hawbReport);
        reportsMap.put(ReportConstants.MAWB, mawbReport);
        reportsMap.put(ReportConstants.CARGO_MANIFEST, cargoManifestReport);
        reportsMap.put(ReportConstants.COMMERCIAL_INVOICE, commercialInvoiceReport);
        reportsMap.put(ReportConstants.CONSOLIDATED_PACKING_LIST, consolidatedPackingListReport);
        reportsMap.put(ReportConstants.FREIGHT_CERTIFICATION, freightCertificationReport);
        reportsMap.put(ReportConstants.EXPORT_CONSOL_MANIFEST, manifestConsolReport);
        reportsMap.put(ReportConstants.IMPORT_SHIPMENT_MANIFEST, manifestShipmentReport);
        reportsMap.put(ReportConstants.EXPORT_SHIPMENT_MANIFEST, manifestShipmentReport);
        reportsMap.put(ReportConstants.GENERATE_ISF_FILE, manifestShipmentReport);
        reportsMap.put(ReportConstants.AWB_LABEL, awbLabelReport);
        reportsMap.put(ReportConstants.SHIPPING_REQUEST_AIR, shippingRequestAirReport);
        reportsMap.put(ReportConstants.PICKUP_ORDER, pickupOrderReport);
        reportsMap.put(ReportConstants.PROOF_OF_DELIVERY, proofOfDeliveryReport);
        reportsMap.put(ReportConstants.PACKING_LIST, packingListReport);
        reportsMap.put(ReportConstants.SHIPMENT_CAN_DOCUMENT, shipmentCANReport);
        reportsMap.put(ReportConstants.CUSTOMS_INSTRUCTIONS, customsInstructionsReport);
        reportsMap.put(ReportConstants.PRE_ALERT, preAlertReport);
        reportsMap.put(ReportConstants.SHIPPING_REQUEST, shippingRequestOutReport);
        reportsMap.put(ReportConstants.MANIFEST_PRINT, manifestPrintReport);
        reportsMap.put(ReportConstants.CONTAINER_MANIFEST_PRINT, containerManifestPrint);
        reportsMap.put(ReportConstants.SEAWAY_BILL, seawayBillReport);
        reportsMap.put(ReportConstants.SHIP_TRUCKWAY_BILL, shipTruckwayBillReport);
        reportsMap.put(ReportConstants.CONS_TRUCKWAY_BILL, consTruckwayBillReport);
        reportsMap.put(ReportConstants.SHIP_TRUCK_DRIVER_PROOF, shipTruckDriverProof);
        reportsMap.put(ReportConstants.CONS_TRUCK_DRIVER_PROOF, consTruckDriverProof);
        reportsMap.put(ReportConstants.TRANSPORT_ORDER, transportOrderReport);
        reportsMap.put(ReportConstants.CARGO_MANIFEST_AIR_IMPORT_SHIPMENT, cargoManifestAirShipmentReport);
        reportsMap.put(ReportConstants.CARGO_MANIFEST_AIR_EXPORT_SHIPMENT, cargoManifestAirShipmentReport);
        reportsMap.put(ReportConstants.CARGO_MANIFEST_AIR_IMPORT_CONSOLIDATION, cargoManifestAirConsolidationReport);
        reportsMap.put(ReportConstants.CARGO_MANIFEST_AIR_EXPORT_CONSOLIDATION, cargoManifestAirConsolidationReport);
        reportsMap.put(ReportConstants.BOOKING_ORDER, bookingOrderReport);
        reportsMap.put(ReportConstants.CSD_REPORT, csdReport);
        reportsMap.put(ReportConstants.FCR_DOCUMENT, fcrDocumentReport);
    }

    public IReport getReport(String key) {
        return reportsMap.get(key);
    }
}
