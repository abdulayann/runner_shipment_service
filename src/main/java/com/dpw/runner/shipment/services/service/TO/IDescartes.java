package com.dpw.runner.shipment.services.service.TO;


import com.dpw.runner.shipment.services.service.TO.request.AwbData;

public interface IDescartes<X, Y> {

     void process(AwbData data);

       Y convertPayload(AwbData data);

}
