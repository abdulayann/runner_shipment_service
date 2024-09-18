package com.dpw.runner.shipment.services.service.TO;


public interface IDescartes<X, Y> {

     void process(X data);

       Y convertPayload(X data);

}
