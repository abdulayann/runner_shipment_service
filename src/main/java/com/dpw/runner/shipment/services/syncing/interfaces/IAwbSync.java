package com.dpw.runner.shipment.services.syncing.interfaces;


import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.syncing.Entity.SaveStatus;

public interface IAwbSync {
    void sync(Awb awb, SaveStatus saveStatus);

}
