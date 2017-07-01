
class Player(object):
    def __init__(self, name):
        self.name = name
        self.tokens = {}
        self.cards = []
        self.discounts = defaultdict(lambda: 0)
        self.score = 0
        self.nobles = []

    def can_claim_noble(self, noble):
        for color, count in self.discounts:
            if count < noble.requirement.get(color, 0):
                return False
        return True

    def claim_nobles(self, nobles):
        '''
        Given a set of nobles,
        return nobles that are unclaimed
        and update self with claimed nobles.
        '''
        unclaimed = [noble for noble in nobles if not self.can_claim_noble(noble)]
        claimed = [noble for noble in nobles if self.can_claim_noble(noble)]

        self.score += sum(map(lambda n: n.score, claimed))
        self.extend(claimed)
        return unclaimed

    def __repr__(self):
        return self.name

    def can_purchase(self, card):
        golds_needed = 0
        for color, cost in card.discounted_cost(self.discounts):
            deficit = max(0, self.tokens.get(color, 0) - cost)
            golds_needed += deficit
        return golds_needed <= self.tokens.get('GOLD', 0)

    def purchase(self, card):
        if not self.can_purchase(card):
            raise RuntimeException('Unchecked purchase() of %s by %s' % (card, self))

        golds_used = 0
        for color, cost in card.discounted_cost(self.discounts):
            tokens_used = min(cost, self.tokens.get(color, 0))
            golds_used += cost - tokens_used

            if tokens_used > 0:
                self.tokens[color] -= tokens_used
        if golds_used > 0:
            self.tokens['GOLD'] -= golds_used

        self.discounts[card.color] += 1
        self.cards.append(card)
        self.score += card.score

