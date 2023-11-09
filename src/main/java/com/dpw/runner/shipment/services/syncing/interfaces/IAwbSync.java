package com.dpw.runner.shipment.services.syncing.interfaces;


import com.dpw.runner.shipment.services.entity.Awb;
import org.springframework.http.ResponseEntity;

public interface IAwbSync {
    ResponseEntity<?> sync(Awb awb);

}
