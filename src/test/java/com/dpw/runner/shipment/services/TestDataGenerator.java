package com.dpw.runner.shipment.services;

import com.dpw.runner.shipment.services.entity.BlDetails;
import com.dpw.runner.shipment.services.entity.MeasurementDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.repository.interfaces.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Component
public class TestDataGenerator {
    public IShipmentDao shipmentDao;
    private IBlDetailsDao blDetailsDao;
    private IMeasurementDao measurementDao;
    private ICarrierDao carrierDao;
    private IPartiesDao partiesDao;

    @Autowired
    public TestDataGenerator(IShipmentDao shipmentDao,
                             IBlDetailsDao blDetailsDao,
                             IMeasurementDao measurementDao,
                             ICarrierDao carrierDao,
                             IPartiesDao partiesDao) {
        this.shipmentDao = shipmentDao;
        this.blDetailsDao = blDetailsDao;
        this.measurementDao = measurementDao;
        this.carrierDao = carrierDao;
        this.partiesDao = partiesDao;
    }

    public List<ShipmentDetails> populateH2WithTestData() {
        List<ShipmentDetails> response = new ArrayList<>();
        ShipmentDetails shipmentDetail1 = ShipmentDetails.builder()
//                .id(1L)
                .guid(UUID.randomUUID())
                .status(1)
                .direction("EXP")
                .source("API")
                .transportMode("SEA")
                .shipmentType("FCL")
                .houseBill("FTOGI1283602230TzrNGM")
                .masterBill("9g4e5ayd93")
                .bookingReference("0F3RY53NJH")
                .consolRef("13073QR1N5")
                .paymentTerms("C1E")
                .goodsDescription("8WM13OLH9R")
                .additionalTerms("ACJ6O7ZVX2")
                .build();
        shipmentDetail1.setTenantId(1);

        BlDetails blDetail1 = BlDetails.builder()
                .guid(UUID.randomUUID())
                .releaseType("3RX")
                .hblType("DEX")
                .screeningStatus("NOW")
                .deliveryMode("SEA")
                .build();
        blDetail1.setTenantId(1);
        blDetailsDao.save(blDetail1);
        shipmentDetail1.setBlDetails(blDetail1);

        MeasurementDetails measurementDetail1 = MeasurementDetails.builder()
                .guid(UUID.randomUUID())
                .weight(null)
                .weightUnit(null)
                .build();
        measurementDetail1.setTenantId(1);
        measurementDao.save(measurementDetail1);
        shipmentDetail1.setMeasurementDetails(measurementDetail1);
        shipmentDao.save(shipmentDetail1);
        response.add(shipmentDetail1);

        //**************END

        ShipmentDetails shipmentDetail2 = ShipmentDetails.builder()
//                .id(2L)
                .guid(UUID.randomUUID())
                .status(0)
                .direction("EXP")
                .source("API")
                .transportMode("AIR")
                .shipmentType("LCL")
                .houseBill("FT0TzrNGM")
                .masterBill("12344AYD93")
                .bookingReference("0F3RY5333H")
                .consolRef("13073QR1N5")
                .paymentTerms("C1E")
                .goodsDescription("8W2323OLH9R")
                .additionalTerms("ABCD6O7ZVX2")
                .build();
        shipmentDetail2.setTenantId(1);

        BlDetails blDetail2 = BlDetails.builder()
                .guid(UUID.randomUUID())
                .releaseType("3RC")
                .hblType("DEX")
                .screeningStatus("NOW")
                .deliveryMode("AIR")
                .build();
        blDetail2.setTenantId(1);
        blDetailsDao.save(blDetail2);
        shipmentDetail2.setBlDetails(blDetail2);

        MeasurementDetails measurementDetail2 = MeasurementDetails.builder()
                .guid(UUID.randomUUID())
                .weight(null)
                .weightUnit(null)
                .build();
        measurementDetail2.setTenantId(1);
        measurementDao.save(measurementDetail2);
        shipmentDetail2.setMeasurementDetails(measurementDetail2);
        shipmentDao.save(shipmentDetail2);
        response.add(shipmentDetail2);

        //*************END

        ShipmentDetails shipmentDetail3 = ShipmentDetails.builder()
//                .id(3L)
                .guid(UUID.randomUUID())
                .status(0)
                .direction("EXP")
                .source("API")
                .transportMode("AIR")
                .shipmentType("LCL")
                .houseBill("FTOGI1283602230TzrNGM")
                .masterBill("12344AYD94")
                .bookingReference("0F3RY5334H")
                .consolRef("13073QR1N6")
                .paymentTerms("C4E")
                .goodsDescription("8W2323OLH0R")
                .additionalTerms("ABCD7O7ZVX2")
                .build();
        shipmentDetail3.setTenantId(1);

        BlDetails blDetail3 = BlDetails.builder()
                .guid(UUID.randomUUID())
                .releaseType("4RC")
                .hblType("DEX")
                .screeningStatus("NOW")
                .deliveryMode("AIR")
                .build();
        blDetail3.setTenantId(1);
        blDetailsDao.save(blDetail3);
        shipmentDetail3.setBlDetails(blDetail3);

        MeasurementDetails measurementDetail3 = MeasurementDetails.builder()
                .guid(UUID.randomUUID())
                .weight(null)
                .weightUnit(null)
                .build();
        measurementDetail3.setTenantId(1);
        measurementDao.save(measurementDetail3);
        shipmentDetail3.setMeasurementDetails(measurementDetail3);
        shipmentDao.save(shipmentDetail3);
        response.add(shipmentDetail3);

        //*************END

        ShipmentDetails shipmentDetail4 = ShipmentDetails.builder()
//                .id(4L)
                .guid(UUID.randomUUID())
                .status(0)
                .direction("EXP")
                .source("Runner")
                .transportMode("AIR")
                .shipmentType("LCL")
                .houseBill("FT0TzrNG4")
                .masterBill("12344AYD95")
                .bookingReference("0F3RY5335H")
                .consolRef("13073QR1N9")
                .paymentTerms("C5E")
                .goodsDescription("0W2323OLH1R")
                .additionalTerms("ABCD7O7ZVX5")
                .build();
        shipmentDetail4.setTenantId(1);

        BlDetails blDetail4 = BlDetails.builder()
                .guid(UUID.randomUUID())
                .releaseType("5RC")
                .hblType("DEX")
                .screeningStatus("NOW")
                .deliveryMode("RAIL")
                .build();
        blDetail4.setTenantId(1);
        blDetailsDao.save(blDetail4);
        shipmentDetail4.setBlDetails(blDetail4);

        MeasurementDetails measurementDetail4 = MeasurementDetails.builder()
                .guid(UUID.randomUUID())
                .weight(null)
                .weightUnit(null)
                .build();
        measurementDetail4.setTenantId(1);
        measurementDao.save(measurementDetail4);
        shipmentDetail4.setMeasurementDetails(measurementDetail4);
        shipmentDao.save(shipmentDetail4);
        response.add(shipmentDetail4);

        //****END

        ShipmentDetails shipmentDetail5 = ShipmentDetails.builder()
//                .id(5L)
                .guid(UUID.randomUUID())
                .shipmentId("SHP000102015")
                .status(0)
                .direction("EXP")
                .source("API")
                .transportMode("SEA")
                .shipmentType("FCL")
                .houseBill("FTOGI128360230TzrNGM")
                .masterBill("9G4E5AYD923")
                .bookingReference("0F3RY53NJH")
                .consolRef("13073QR1N5")
                .paymentTerms("C2E")
                .goodsDescription("8WM13OLH0R")
                .additionalTerms("ACJ6O7ZVX4")
                .build();
        shipmentDetail5.setTenantId(1);

        BlDetails blDetail5 = BlDetails.builder()
                .guid(UUID.randomUUID())
                .releaseType("5RX")
                .hblType("DEX")
                .screeningStatus("NOW")
                .deliveryMode("SEA")
                .build();
        blDetail5.setTenantId(1);
        blDetailsDao.save(blDetail5);
        shipmentDetail5.setBlDetails(blDetail5);

        MeasurementDetails measurementDetail5 = MeasurementDetails.builder()
                .guid(UUID.randomUUID())
                .weight(null)
                .weightUnit(null)
                .build();
        measurementDetail5.setTenantId(1);
        measurementDao.save(measurementDetail5);
        shipmentDetail5.setMeasurementDetails(measurementDetail5);
        shipmentDao.save(shipmentDetail5);
        response.add(shipmentDetail5);

        return response;
    }


}
