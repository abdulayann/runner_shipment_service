package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.HblLockSettings;

public interface IHblLockSettingsRepository extends MultiTenancyRepository<HblLockSettings> {
}
