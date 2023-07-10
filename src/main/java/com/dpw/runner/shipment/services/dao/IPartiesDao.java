package com.dpw.runner.shipment.services.dao;

import com.dpw.runner.shipment.services.entity.Parties;

import java.util.List;

public interface IPartiesDao {
    List<Parties> saveAll(List<Parties> parties);
}
